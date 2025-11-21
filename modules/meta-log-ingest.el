;;; meta-log-ingest.el --- One-click folder ingestion for meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne <bthornemail@gmail.com>

;;; Commentary:
;; Simple "Add my notes folder" functionality that runs the entire
;; ingestion pipeline: file scanning → text extraction → LLM processing →
;; knowledge graph building → done.

;;; Code:

(require 'meta-log-knowledge-graph nil t)
(require 'meta-log-llm nil t)

(defvar meta-log-notes-folder nil
  "Path to the user's notes folder.")

(defvar meta-log-ingest-supported-extensions
  '("md" "org" "txt")
  "File extensions to ingest (without dots).")

(defvar meta-log-ingest-progress-buffer "*meta-log Ingestion*")

;;;###autoload
(defun meta-log-ingest-folder (folder)
  "Ingest all supported files from FOLDER into the knowledge graph.
This is the one-click 'magic' function that:
1. Scans folder for supported files
2. Extracts content and metadata
3. Builds knowledge graph
4. Enables AI-powered search"
  (interactive "DSelect notes folder to import: ")

  (unless (file-directory-p folder)
    (user-error "Not a directory: %s" folder))

  (setq meta-log-notes-folder folder)

  ;; Show progress buffer
  (let ((progress-buf (get-buffer-create meta-log-ingest-progress-buffer)))
    (with-current-buffer progress-buf
      (erase-buffer)
      (insert (propertize "╔════════════════════════════════════════════════════════════╗\n"
                          'face '(:foreground "cyan")))
      (insert (propertize "║      📥  Importing Notes into meta-log  📥                ║\n"
                          'face '(:foreground "cyan")))
      (insert (propertize "╚════════════════════════════════════════════════════════════╝\n\n"
                          'face '(:foreground "cyan")))
      (insert (format "Folder: %s\n\n" folder)))
    (display-buffer progress-buf))

  ;; Find all supported files
  (meta-log-ingest-log "🔍 Scanning for files...")
  (let* ((files (meta-log-ingest-find-files folder))
         (total (length files))
         (processed 0))

    (meta-log-ingest-log (format "   Found %d files\n" total))

    ;; Initialize knowledge graph if needed
    (when (require 'meta-log-knowledge-graph nil t)
      (unless (boundp 'meta-log-kg-nodes)
        (meta-log-kg-init)))

    ;; Process each file
    (meta-log-ingest-log "\n📚 Processing files...")
    (dolist (file files)
      (setq processed (1+ processed))
      (meta-log-ingest-log (format "   [%d/%d] %s"
                                   processed total
                                   (file-name-nondirectory file)))

      (condition-case err
          (meta-log-ingest-file file)
        (error
         (meta-log-ingest-log (format " ❌ ERROR: %s" (error-message-string err)))))

      (meta-log-ingest-log " ✓\n"))

    ;; Build knowledge graph connections
    (meta-log-ingest-log "\n🔗 Building knowledge graph...")
    (when (fboundp 'meta-log-kg-build-connections)
      (meta-log-kg-build-connections))
    (meta-log-ingest-log " ✓\n")

    ;; Done!
    (meta-log-ingest-log
     (format "\n%s Import Complete! %s\n\n"
             (propertize "✨" 'face '(:foreground "green"))
             (propertize "✨" 'face '(:foreground "green"))))
    (meta-log-ingest-log (format "  • Processed: %d files\n" total))
    (when (boundp 'meta-log-kg-nodes)
      (meta-log-ingest-log (format "  • Knowledge nodes: %d\n"
                                   (hash-table-count meta-log-kg-nodes)))
      (meta-log-ingest-log (format "  • Connections: %d\n"
                                   (hash-table-count meta-log-kg-edges))))

    (meta-log-ingest-log "\n💡 Try: M-x meta-log-chat\n")

    (message "✓ Import complete! %d files processed" total)))

(defun meta-log-ingest-find-files (folder)
  "Find all supported files in FOLDER recursively."
  (let ((files '()))
    (dolist (ext meta-log-ingest-supported-extensions)
      (setq files
            (append files
                    (directory-files-recursively
                     folder
                     (concat "\\." ext "$")))))
    files))

(defun meta-log-ingest-file (file)
  "Ingest a single FILE into the knowledge graph."
  (let* ((content (meta-log-ingest-read-file file))
         (metadata (meta-log-ingest-extract-metadata file content))
         (node-id (meta-log-ingest-file-to-node-id file)))

    ;; Add to knowledge graph
    (when (fboundp 'meta-log-kg-add-node)
      (meta-log-kg-add-node
       node-id
       `((type . file)
         (path . ,file)
         (content . ,content)
         (title . ,(alist-get 'title metadata))
         (tags . ,(alist-get 'tags metadata))
         (modified . ,(alist-get 'modified metadata)))))

    ;; Extract entities and add connections (if LLM available)
    (when (and (fboundp 'meta-log-llm-extract-entities)
               (or (boundp 'meta-log-llm-backend)
                   (getenv "OPENAI_API_KEY")))
      (let ((entities (meta-log-llm-extract-entities content)))
        (dolist (entity entities)
          (when (fboundp 'meta-log-kg-add-edge)
            (meta-log-kg-add-edge node-id entity "mentions")))))))

(defun meta-log-ingest-read-file (file)
  "Read content from FILE, handling different formats."
  (with-temp-buffer
    (insert-file-contents file)
    (pcase (file-name-extension file)
      ("org"
       ;; For Org files, extract plain text (skip markup)
       (when (require 'org nil t)
         (org-mode)
         (buffer-substring-no-properties (point-min) (point-max))))
      ("md"
       ;; For Markdown, return as-is for now
       (buffer-substring-no-properties (point-min) (point-max)))
      (_
       ;; Plain text
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun meta-log-ingest-extract-metadata (file content)
  "Extract metadata from FILE and its CONTENT."
  (list
   (cons 'title (meta-log-ingest-guess-title file content))
   (cons 'tags (meta-log-ingest-extract-tags content))
   (cons 'modified (file-attribute-modification-time (file-attributes file)))))

(defun meta-log-ingest-guess-title (file content)
  "Guess the title from FILE or CONTENT."
  (or
   ;; Try to find title in content
   (when (string-match "^#\\s-+\\(.+\\)$" content)
     (match-string 1 content))
   (when (string-match "^\\*\\s-+\\(.+\\)$" content)
     (match-string 1 content))
   ;; Fall back to filename
   (file-name-sans-extension (file-name-nondirectory file))))

(defun meta-log-ingest-extract-tags (content)
  "Extract tags from CONTENT."
  (let ((tags '()))
    ;; Org-style tags
    (when (string-match "^#\\+TAGS:\\s-*\\(.+\\)$" content)
      (setq tags (split-string (match-string 1 content) "\\s-+")))
    ;; Hashtags
    (let ((pos 0))
      (while (string-match "#\\([a-zA-Z0-9_-]+\\)" content pos)
        (push (match-string 1 content) tags)
        (setq pos (match-end 0))))
    (delete-dups tags)))

(defun meta-log-ingest-file-to-node-id (file)
  "Convert FILE path to a stable node ID."
  (concat "file:" (secure-hash 'sha256 file)))

(defun meta-log-ingest-log (message)
  "Log MESSAGE to the ingestion progress buffer."
  (with-current-buffer (get-buffer-create meta-log-ingest-progress-buffer)
    (goto-char (point-max))
    (insert message)
    ;; Force redisplay so user sees progress
    (redisplay)))

;; File watcher for auto-import
(defvar meta-log-ingest-file-watcher nil
  "File watcher descriptor for auto-import.")

(defun meta-log-ingest-watch-folder ()
  "Watch notes folder for changes and auto-import new files."
  (when (and meta-log-notes-folder
             (boundp 'meta-log-auto-ingest)
             meta-log-auto-ingest)
    (require 'filenotify)
    (setq meta-log-ingest-file-watcher
          (file-notify-add-watch
           meta-log-notes-folder
           '(change)
           'meta-log-ingest-file-changed))))

(defun meta-log-ingest-file-changed (event)
  "Handle file change EVENT for auto-import."
  (let ((file (nth 2 event))
        (action (nth 1 event)))
    (when (and (memq action '(created changed))
               (member (file-name-extension file)
                       meta-log-ingest-supported-extensions))
      (message "meta-log: Auto-importing %s..." (file-name-nondirectory file))
      (meta-log-ingest-file file))))

(provide 'meta-log-ingest)

;;; meta-log-ingest.el ends here
