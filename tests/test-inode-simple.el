;;; test-inode-simple.el --- Simple inode demonstration

(add-to-list 'load-path "/data/data/com.termux/files/home/github/meta-log")
(require 'meta-log-inode)

(princ "\n")
(princ "╔══════════════════════════════════════════════════════════╗\n")
(princ "║  INODE-BASED FILE ADDRESSING DEMONSTRATION              ║\n")
(princ "╚══════════════════════════════════════════════════════════╝\n")
(princ "\n")

;; Test 1: Register meta-log files
(princ "TEST 1: Registering meta-log Files\n")
(princ "===================================\n\n")

(dolist (file '("meta-log.el" "meta-log-inode.el" "meta-log-knowledge-graph.el"))
  (let* ((path (expand-file-name file "/data/data/com.termux/files/home/github/meta-log"))
         (ref (meta-log-inode-register-file path)))
    (when ref
      (princ (format "%-35s inode: %7d  device: %d\n"
                     file
                     (meta-log-inode-ref-inode ref)
                     (meta-log-inode-ref-device ref))))))

(princ "\n")

;; Test 2: Demonstrate rename resilience
(princ "TEST 2: Rename Resilience\n")
(princ "=========================\n\n")

(let ((original-path (expand-file-name "test-rename-demo.txt" "/data/data/com.termux/files/home/github/meta-log/examples"))
      (renamed-path (expand-file-name "test-RENAMED.txt" "/data/data/com.termux/files/home/github/meta-log/examples")))

  ;; Cleanup from previous runs
  (when (file-exists-p original-path) (delete-file original-path))
  (when (file-exists-p renamed-path) (delete-file renamed-path))

  ;; Create and register file
  (with-temp-file original-path
    (insert "This content will survive rename!"))

  (let* ((ref (meta-log-inode-register-file original-path))
         (inode (meta-log-inode-ref-inode ref))
         (device (meta-log-inode-ref-device ref)))

    (princ (format "Created: %s\n" (file-name-nondirectory original-path)))
    (princ (format "  Inode: %d\n" inode))
    (princ (format "  Device: %d\n\n" device))

    ;; Rename the file
    (rename-file original-path renamed-path t)
    (princ (format "Renamed to: %s\n\n" (file-name-nondirectory renamed-path)))

    ;; Can we still access it by inode?
    (let ((resolved-path (meta-log-inode-resolve (cons device inode))))
      (princ (format "Resolution by (device . inode):\n"))
      (princ (format "  Key: (%d . %d)\n" device inode))
      (princ (format "  Resolved path: %s\n"
                     (if resolved-path
                         (file-name-nondirectory resolved-path)
                       "FAILED")))
      (princ (format "  Success: %s\n\n" (if resolved-path "YES ✓" "NO")))

      ;; Read content via inode (even though filename changed!)
      (when resolved-path
        (let ((content (meta-log-inode-read-file (cons device inode))))
          (princ (format "Content via inode addressing:\n"))
          (princ (format "  \"%s\"\n\n" (string-trim content))))))

    ;; Cleanup
    (when (file-exists-p renamed-path)
      (delete-file renamed-path))))

;; Test 3: Deduplication detection
(princ "TEST 3: Duplicate Content Detection\n")
(princ "====================================\n\n")

(let ((file1 (expand-file-name "dup1.txt" "/data/data/com.termux/files/home/github/meta-log/examples"))
      (file2 (expand-file-name "dup2.txt" "/data/data/com.termux/files/home/github/meta-log/examples"))
      (file3 (expand-file-name "unique.txt" "/data/data/com.termux/files/home/github/meta-log/examples")))

  ;; Cleanup
  (dolist (f (list file1 file2 file3))
    (when (file-exists-p f) (delete-file f)))

  ;; Create files with duplicate content
  (with-temp-file file1
    (insert "Identical content in both files"))
  (with-temp-file file2
    (insert "Identical content in both files"))
  (with-temp-file file3
    (insert "Completely different content"))

  ;; Register all
  (let ((ref1 (meta-log-inode-register-file file1))
        (ref2 (meta-log-inode-register-file file2))
        (ref3 (meta-log-inode-register-file file3)))

    (princ (format "Created 3 files:\n"))
    (princ (format "  %s (inode: %d)\n"
                   (file-name-nondirectory file1)
                   (meta-log-inode-ref-inode ref1)))
    (princ (format "  %s (inode: %d)\n"
                   (file-name-nondirectory file2)
                   (meta-log-inode-ref-inode ref2)))
    (princ (format "  %s (inode: %d)\n\n"
                   (file-name-nondirectory file3)
                   (meta-log-inode-ref-inode ref3))))

  ;; Find duplicates
  (let ((duplicates (meta-log-inode-find-duplicates)))
    (princ (format "Duplicate detection results:\n"))
    (princ (format "  Found %d sets of duplicate content\n\n" (length duplicates)))
    (dolist (dup duplicates)
      (princ (format "  Content hash: %s...\n" (substring (car dup) 0 16)))
      (princ (format "  Files with same content:\n"))
      (dolist (path (cdr dup))
        (princ (format "    - %s\n" (file-name-nondirectory path))))
      (princ "\n")))

  ;; Cleanup
  (dolist (f (list file1 file2 file3))
    (when (file-exists-p f) (delete-file f))))

;; Test 4: Registry statistics
(princ "TEST 4: Registry Statistics\n")
(princ "===========================\n\n")
(meta-log-inode-registry-stats)

(princ "\n")
(princ "╔══════════════════════════════════════════════════════════╗\n")
(princ "║  KEY ADVANTAGES                                         ║\n")
(princ "╚══════════════════════════════════════════════════════════╝\n")
(princ "\n")
(princ "1. RENAME RESILIENCE\n")
(princ "   Files remain accessible even after rename/move\n")
(princ "   Knowledge graph links survive reorganization\n\n")

(princ "2. CONTENT-ADDRESSABLE\n")
(princ "   Files referenced by content (inode), not arbitrary names\n")
(princ "   Provides Git-like immutable references\n\n")

(princ "3. DEDUPLICATION\n")
(princ "   Automatically detect duplicate content\n")
(princ "   Even across different directories\n\n")

(princ "4. TOPOLOGICAL INVARIANCE\n")
(princ "   File identity independent of filesystem structure\n")
(princ "   Perfect for meta-log's topological approach\n\n")

(princ "5. HARD LINK DETECTION\n")
(princ "   Know when multiple paths → same content\n")
(princ "   Track all references to a file\n\n")

(provide 'test-inode-simple)
