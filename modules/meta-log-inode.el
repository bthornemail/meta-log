;;; meta-log-inode.el --- Inode-based file addressing for meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Access files by inode rather than path for:
;; - Content-addressable storage
;; - Rename/move resilience
;; - Deduplication detection
;; - Hard link awareness
;; - Topological invariance

;;; Code:

(require 'cl-lib)

;;; Core Inode Operations

(cl-defstruct meta-log-inode-ref
  "Reference to a file by its inode."
  inode          ; Inode number
  device         ; Device number (to handle multiple filesystems)
  paths          ; List of all known paths to this inode
  content-hash   ; Optional hash of content for verification
  metadata       ; Plist of file metadata
  last-seen)     ; Timestamp of last access

(defvar meta-log-inode--registry (make-hash-table :test 'equal)
  "Registry mapping (device . inode) to meta-log-inode-ref structures.")

(defvar meta-log-inode--path-index (make-hash-table :test 'equal)
  "Reverse index mapping paths to (device . inode) keys.")

;;; File Information Extraction

(defun meta-log-inode-get-info (file-path)
  "Get inode information for FILE-PATH.
Returns plist with :inode, :device, :nlink, :size, :mtime."
  (when (file-exists-p file-path)
    (let* ((attrs (file-attributes file-path))
           (stat-output (shell-command-to-string
                         (format "stat -c '%%i %%d %%h %%s %%Y' %s"
                                 (shell-quote-argument file-path))))
           (parts (split-string (string-trim stat-output))))
      (when (= (length parts) 5)
        (list :inode (string-to-number (nth 0 parts))
              :device (string-to-number (nth 1 parts))
              :nlink (string-to-number (nth 2 parts))
              :size (string-to-number (nth 3 parts))
              :mtime (string-to-number (nth 4 parts)))))))

(defun meta-log-inode-register-file (file-path)
  "Register FILE-PATH in inode registry.
Returns inode-ref structure."
  (let* ((info (meta-log-inode-get-info file-path))
         (inode (plist-get info :inode))
         (device (plist-get info :device))
         (key (cons device inode))
         (existing (gethash key meta-log-inode--registry)))

    (if existing
        ;; Update existing entry with new path
        (progn
          (unless (member file-path (meta-log-inode-ref-paths existing))
            (push file-path (meta-log-inode-ref-paths existing)))
          (setf (meta-log-inode-ref-last-seen existing) (current-time))
          existing)

      ;; Create new entry
      (let ((ref (make-meta-log-inode-ref
                  :inode inode
                  :device device
                  :paths (list file-path)
                  :metadata info
                  :last-seen (current-time))))
        (puthash key ref meta-log-inode--registry)
        (puthash file-path key meta-log-inode--path-index)
        ref))))

;;; Inode-Based File Access

(defun meta-log-inode-resolve (inode-or-path)
  "Resolve INODE-OR-PATH to current file path.
INODE-OR-PATH can be:
  - String path
  - (device . inode) cons
  - meta-log-inode-ref structure
Returns most recently valid path, or nil if file no longer exists."
  (let* ((ref (cond
               ((stringp inode-or-path)
                ;; Lookup by path
                (let ((key (gethash inode-or-path meta-log-inode--path-index)))
                  (when key (gethash key meta-log-inode--registry))))

               ((consp inode-or-path)
                ;; Direct (device . inode) lookup
                (gethash inode-or-path meta-log-inode--registry))

               ((meta-log-inode-ref-p inode-or-path)
                ;; Already have the ref
                inode-or-path)

               (t nil))))

    (when ref
      ;; Try each known path until we find one that still exists
      ;; and has the same inode
      (let ((valid-path nil)
            (inode (meta-log-inode-ref-inode ref))
            (device (meta-log-inode-ref-device ref)))
        (dolist (path (meta-log-inode-ref-paths ref))
          (when (and (file-exists-p path)
                     (not valid-path))
            (let ((current-info (meta-log-inode-get-info path)))
              (when (and current-info
                        (= (plist-get current-info :inode) inode)
                        (= (plist-get current-info :device) device))
                (setq valid-path path)))))

        ;; If no known path works, search parent directory for moved file
        (unless valid-path
          (let ((dir (file-name-directory (car (meta-log-inode-ref-paths ref)))))
            (when (file-directory-p dir)
              (dolist (file (directory-files dir t "^[^.]"))
                (when (and (file-regular-p file)
                          (not valid-path))
                  (let ((file-info (meta-log-inode-get-info file)))
                    (when (and file-info
                              (= (plist-get file-info :inode) inode)
                              (= (plist-get file-info :device) device))
                      (setq valid-path file)
                      ;; Update registry with new path
                      (push file (meta-log-inode-ref-paths ref))
                      (puthash file (cons device inode) meta-log-inode--path-index))))))))

        valid-path))))

(defun meta-log-inode-read-file (inode-or-path)
  "Read file contents using inode-based addressing.
INODE-OR-PATH resolved via meta-log-inode-resolve."
  (let ((path (meta-log-inode-resolve inode-or-path)))
    (when path
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

;;; Hard Link Detection

(defun meta-log-inode-find-hard-links (file-path)
  "Find all hard links to FILE-PATH.
Returns list of paths pointing to same inode."
  (let* ((info (meta-log-inode-get-info file-path))
         (nlink (plist-get info :nlink)))
    (when (> nlink 1)
      ;; File has hard links
      (let* ((inode (plist-get info :inode))
             (device (plist-get info :device))
             (key (cons device inode))
             (ref (gethash key meta-log-inode--registry)))
        (when ref
          (meta-log-inode-ref-paths ref))))))

;;; Deduplication

(defun meta-log-inode-find-duplicates ()
  "Find files with same content but different inodes.
Returns alist of (content-hash . (list of paths))."
  (let ((hash-to-paths (make-hash-table :test 'equal))
        (duplicates '()))

    ;; Hash all registered files
    (maphash (lambda (key ref)
               (let ((path (car (meta-log-inode-ref-paths ref))))
                 (when (and path (file-exists-p path))
                   (let ((hash (secure-hash 'sha256
                                           (with-temp-buffer
                                             (insert-file-contents path)
                                             (buffer-string)))))
                     ;; Store hash in ref
                     (setf (meta-log-inode-ref-content-hash ref) hash)

                     ;; Track paths by hash
                     (push (cons (meta-log-inode-ref-inode ref) path)
                           (gethash hash hash-to-paths))))))
             meta-log-inode--registry)

    ;; Find hashes with multiple inodes
    (maphash (lambda (hash inode-paths)
               (when (> (length inode-paths) 1)
                 (push (cons hash (mapcar #'cdr inode-paths)) duplicates)))
             hash-to-paths)

    duplicates))

;;; Track File Moves/Renames

(defun meta-log-inode-track-moves ()
  "Detect when registered files have been moved/renamed.
Returns alist of (old-path . new-path)."
  (let ((moves '()))
    (maphash (lambda (key ref)
               (let ((old-paths (meta-log-inode-ref-paths ref))
                     (current-path (meta-log-inode-resolve ref)))
                 (when (and current-path
                           (not (member current-path old-paths)))
                   ;; Found file at new location
                   (push (cons (car old-paths) current-path) moves)
                   ;; Update registry
                   (push current-path (meta-log-inode-ref-paths ref)))))
             meta-log-inode--registry)
    moves))

;;; Integration with Knowledge Graph

(defun meta-log-inode-kg-register-document (file-path)
  "Register document in both knowledge graph and inode registry.
Returns (kg-node-id . inode-ref)."
  (require 'meta-log-knowledge-graph)

  (let* ((inode-ref (meta-log-inode-register-file file-path))
         (kg-node (meta-log-kg-ingest-file file-path)))

    ;; Link them together
    (when (and inode-ref kg-node)
      ;; Store inode info in KG node metadata
      (setf (meta-log-kg-node-properties kg-node)
            (plist-put (meta-log-kg-node-properties kg-node)
                      :inode (meta-log-inode-ref-inode inode-ref)))
      (setf (meta-log-kg-node-properties kg-node)
            (plist-put (meta-log-kg-node-properties kg-node)
                      :device (meta-log-inode-ref-device inode-ref)))

      (cons (meta-log-kg-node-id kg-node) inode-ref))))

(defun meta-log-inode-kg-query-by-inode (inode device)
  "Query knowledge graph for document with INODE on DEVICE."
  (require 'meta-log-knowledge-graph)

  (let ((found nil))
    (maphash (lambda (id node)
               (when (and (eq (meta-log-kg-node-type node) 'document)
                         (equal (plist-get (meta-log-kg-node-properties node) :inode)
                               inode)
                         (equal (plist-get (meta-log-kg-node-properties node) :device)
                               device))
                 (setq found node)))
            meta-log-kg--document-index)
    found))

;;; Utility Functions

(defun meta-log-inode-registry-stats ()
  "Show statistics about inode registry."
  (interactive)
  (let ((total-refs (hash-table-count meta-log-inode--registry))
        (hard-links 0)
        (total-paths 0))

    (maphash (lambda (key ref)
               (let ((npaths (length (meta-log-inode-ref-paths ref))))
                 (setq total-paths (+ total-paths npaths))
                 (when (> npaths 1)
                   (setq hard-links (1+ hard-links)))))
             meta-log-inode--registry)

    (message "Inode Registry Statistics:")
    (message "  Unique inodes: %d" total-refs)
    (message "  Total paths: %d" total-paths)
    (message "  Hard links: %d" hard-links)
    (message "  Avg paths/inode: %.2f" (/ (float total-paths) (max 1 total-refs)))))

(defun meta-log-inode-clean-stale ()
  "Remove entries for files that no longer exist."
  (interactive)
  (let ((removed 0))
    (maphash (lambda (key ref)
               (unless (meta-log-inode-resolve ref)
                 (remhash key meta-log-inode--registry)
                 (setq removed (1+ removed))))
             meta-log-inode--registry)
    (message "Removed %d stale inode entries" removed)))

(provide 'meta-log-inode)

;;; meta-log-inode.el ends here
