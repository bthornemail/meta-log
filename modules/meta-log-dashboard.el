;;; meta-log-dashboard.el --- Simple dashboard interface for meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne <bthornemail@gmail.com>

;;; Commentary:
;; User-friendly dashboard that replaces the raw Emacs buffer experience
;; Shows: stats, recent queries, quick actions, knowledge graph overview

;;; Code:

(require 'tabulated-list)

(defvar meta-log-dashboard-buffer "*meta-log Dashboard*")

(defvar meta-log-dashboard-stats
  '((files . 0)
    (nodes . 0)
    (connections . 0)
    (last-query . nil))
  "Dashboard statistics.")

;;;###autoload
(defun meta-log-dashboard ()
  "Launch the meta-log dashboard - main entry point for users."
  (interactive)
  (switch-to-buffer meta-log-dashboard-buffer)
  (meta-log-dashboard-mode)
  (meta-log-dashboard-refresh))

(defun meta-log-dashboard-refresh ()
  "Refresh the dashboard display."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Header
    (insert (propertize "
╔══════════════════════════════════════════════════════════════════════╗
║                                                                      ║
║              📚  meta-log  —  Your AI Knowledge System              ║
║                                                                      ║
╚══════════════════════════════════════════════════════════════════════╝
" 'face '(:foreground "cyan" :weight bold)))

    (insert "\n")

    ;; Stats panel
    (insert (propertize "┌─ System Status " 'face '(:foreground "yellow")))
    (insert (propertize (make-string 54 ?─) 'face '(:foreground "yellow")))
    (insert (propertize "┐\n" 'face '(:foreground "yellow")))

    (meta-log-dashboard-update-stats)

    (insert (format "│  📁 Indexed Files:      %s%-10s%s                              │\n"
                    (propertize "" 'face '(:foreground "green"))
                    (number-to-string (alist-get 'files meta-log-dashboard-stats))
                    (propertize "" 'face 'default)))

    (insert (format "│  🔗 Knowledge Nodes:    %s%-10s%s                              │\n"
                    (propertize "" 'face '(:foreground "cyan"))
                    (number-to-string (alist-get 'nodes meta-log-dashboard-stats))
                    (propertize "" 'face 'default)))

    (insert (format "│  ⚡ Connections:        %s%-10s%s                              │\n"
                    (propertize "" 'face '(:foreground "magenta"))
                    (number-to-string (alist-get 'connections meta-log-dashboard-stats))
                    (propertize "" 'face 'default)))

    (let ((backend (if (boundp 'meta-log-llm-backend)
                       (symbol-name meta-log-llm-backend)
                     "not configured")))
      (insert (format "│  🤖 AI Backend:         %s%-40s%s│\n"
                      (propertize "" 'face '(:foreground "yellow"))
                      backend
                      (propertize "" 'face 'default))))

    (insert (propertize "└" 'face '(:foreground "yellow")))
    (insert (propertize (make-string 69 ?─) 'face '(:foreground "yellow")))
    (insert (propertize "┘\n\n" 'face '(:foreground "yellow")))

    ;; Quick actions
    (insert (propertize "┌─ Quick Actions " 'face '(:foreground "green")))
    (insert (propertize (make-string 53 ?─) 'face '(:foreground "green")))
    (insert (propertize "┐\n" 'face '(:foreground "green")))
    (insert "│                                                                     │\n")

    ;; Action buttons
    (insert "│  ")
    (insert-text-button "💬 Ask a Question"
                        'action (lambda (_) (call-interactively 'meta-log-chat))
                        'follow-link t
                        'help-echo "Open chat interface")
    (insert (make-string (- 67 (length "  💬 Ask a Question")) ? ))
    (insert "│\n")

    (insert "│  ")
    (insert-text-button "📥 Import Notes Folder"
                        'action (lambda (_) (call-interactively 'meta-log-ingest-folder))
                        'follow-link t
                        'help-echo "Import and index a folder of notes")
    (insert (make-string (- 67 (length "  📥 Import Notes Folder")) ? ))
    (insert "│\n")

    (insert "│  ")
    (insert-text-button "🔍 Search Knowledge Base"
                        'action (lambda (_) (call-interactively 'meta-log-search))
                        'follow-link t
                        'help-echo "Search all indexed content")
    (insert (make-string (- 67 (length "  🔍 Search Knowledge Base")) ? ))
    (insert "│\n")

    (insert "│  ")
    (insert-text-button "📊 View Knowledge Graph"
                        'action (lambda (_) (meta-log-dashboard-show-graph))
                        'follow-link t
                        'help-echo "Visualize knowledge connections")
    (insert (make-string (- 67 (length "  📊 View Knowledge Graph")) ? ))
    (insert "│\n")

    (insert "│  ")
    (insert-text-button "⚙️  Settings & Configuration"
                        'action (lambda (_) (meta-log-setup))
                        'follow-link t
                        'help-echo "Configure meta-log settings")
    (insert (make-string (- 67 (length "  ⚙️  Settings & Configuration")) ? ))
    (insert "│\n")

    (insert "│                                                                     │\n")
    (insert (propertize "└" 'face '(:foreground "green")))
    (insert (propertize (make-string 69 ?─) 'face '(:foreground "green")))
    (insert (propertize "┘\n\n" 'face '(:foreground "green")))

    ;; Recent activity
    (insert (propertize "┌─ Recent Activity " 'face '(:foreground "magenta")))
    (insert (propertize (make-string 51 ?─) 'face '(:foreground "magenta")))
    (insert (propertize "┐\n" 'face '(:foreground "magenta")))

    (let ((recent (meta-log-dashboard-get-recent-queries)))
      (if recent
          (dolist (query recent)
            (insert (format "│  • %s%s│\n"
                            (truncate-string-to-width query 64 nil nil "...")
                            (make-string (max 0 (- 66 (length query))) ? ))))
        (insert "│  No recent queries yet. Try asking something!                      │\n")))

    (insert (propertize "└" 'face '(:foreground "magenta")))
    (insert (propertize (make-string 69 ?─) 'face '(:foreground "magenta")))
    (insert (propertize "┘\n\n" 'face '(:foreground "magenta")))

    ;; Quick tips
    (insert (propertize "💡 Tips:\n" 'face '(:foreground "yellow" :weight bold)))
    (insert "  • Press 'q' to quit, 'g' to refresh, 'h' for help\n")
    (insert "  • Type '?' to see all keyboard shortcuts\n")
    (insert "  • Click any button above to get started\n")

    (goto-char (point-min))
    (forward-line 10)))

(defun meta-log-dashboard-update-stats ()
  "Update dashboard statistics from the knowledge graph."
  (when (require 'meta-log-knowledge-graph nil t)
    (setf (alist-get 'nodes meta-log-dashboard-stats)
          (hash-table-count meta-log-kg-nodes))
    (setf (alist-get 'connections meta-log-dashboard-stats)
          (hash-table-count meta-log-kg-edges)))

  ;; Count indexed files
  (when (boundp 'meta-log-notes-folder)
    (when (and meta-log-notes-folder (file-directory-p meta-log-notes-folder))
      (setf (alist-get 'files meta-log-dashboard-stats)
            (length (directory-files-recursively
                     meta-log-notes-folder
                     "\\.\\(md\\|org\\|txt\\)$"))))))

(defun meta-log-dashboard-get-recent-queries ()
  "Get list of recent queries."
  (when (boundp 'meta-log-query-history)
    (seq-take meta-log-query-history 5)))

(defun meta-log-dashboard-show-graph ()
  "Show knowledge graph visualization."
  (interactive)
  (if (require 'meta-log-knowledge-graph nil t)
      (meta-log-kg-visualize)
    (message "Knowledge graph not yet built. Import some notes first!")))

(defvar meta-log-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'meta-log-dashboard-refresh)
    (define-key map (kbd "h") 'meta-log-dashboard-help)
    (define-key map (kbd "?") 'meta-log-dashboard-help)
    (define-key map (kbd "c") 'meta-log-chat)
    (define-key map (kbd "i") 'meta-log-ingest-folder)
    (define-key map (kbd "s") 'meta-log-search)
    (define-key map (kbd "v") 'meta-log-dashboard-show-graph)
    (define-key map (kbd "RET") 'push-button)
    map)
  "Keymap for meta-log-dashboard-mode.")

(define-derived-mode meta-log-dashboard-mode special-mode "meta-log Dashboard"
  "Major mode for the meta-log dashboard.

\\{meta-log-dashboard-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun meta-log-dashboard-help ()
  "Show help for dashboard keybindings."
  (interactive)
  (message (concat "Dashboard commands: "
                   "(c)hat | (i)mport | (s)earch | (v)iew graph | "
                   "(g)refresh | (h)elp | (q)uit")))

;; Simple search interface
(defvar meta-log-query-history nil
  "History of user queries.")

(defun meta-log-search (query)
  "Search knowledge base for QUERY."
  (interactive "sSearch for: ")
  (push query meta-log-query-history)

  ;; Use existing grep-based search for now
  (let* ((folder (if (boundp 'meta-log-notes-folder)
                     meta-log-notes-folder
                   default-directory))
         (results (shell-command-to-string
                   (format "grep -r -i -n '%s' %s"
                           (shell-quote-argument query)
                           (shell-quote-argument folder)))))

    (with-current-buffer (get-buffer-create "*meta-log Search Results*")
      (erase-buffer)
      (insert (format "Search results for: %s\n\n" query))
      (insert results)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'meta-log-dashboard)

;;; meta-log-dashboard.el ends here
