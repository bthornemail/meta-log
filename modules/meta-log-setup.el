;;; meta-log-setup.el --- Interactive setup wizard for meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne <bthornemail@gmail.com>

;;; Commentary:
;; First-time setup wizard that makes meta-log usable in < 5 minutes
;; Guides users through:
;; - LLM backend selection (local Ollama vs API)
;; - Notes folder import
;; - Knowledge graph initialization

;;; Code:

(require 'widget)
(require 'wid-edit)

(defvar meta-log-setup-buffer "*meta-log Setup*")
(defvar meta-log-setup-config-file (expand-file-name "~/.meta-log/config.el"))

(defvar meta-log-setup-state
  '((llm-backend . nil)
    (notes-folder . nil)
    (auto-ingest . t)
    (enable-kg . t))
  "Current setup state.")

;;;###autoload
(defun meta-log-setup ()
  "Launch the interactive setup wizard for first-time users."
  (interactive)
  (switch-to-buffer meta-log-setup-buffer)
  (kill-all-local-variables)
  (remove-overlays)
  (meta-log-setup-welcome-screen))

(defun meta-log-setup-welcome-screen ()
  "Display welcome screen."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-insert (propertize "
╔═══════════════════════════════════════════════════════════════╗
║                                                               ║
║        🚀  Welcome to meta-log Setup Wizard  🚀              ║
║                                                               ║
║   Your AI-powered knowledge system in 3 simple steps         ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
" 'face '(:foreground "cyan" :weight bold)))

    (widget-insert "\n")
    (widget-insert "This wizard will help you:\n\n")
    (widget-insert "  ✓ Choose your AI backend (local or cloud)\n")
    (widget-insert "  ✓ Import your existing notes\n")
    (widget-insert "  ✓ Build your knowledge graph\n\n")
    (widget-insert "Time required: " (propertize "~3 minutes" 'face '(:foreground "green" :weight bold)) "\n\n")

    (widget-create 'push-button
                   :notify (lambda (&rest _) (meta-log-setup-step-1-llm))
                   "Let's Get Started!")

    (widget-insert "  ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (kill-buffer meta-log-setup-buffer))
                   "I'll configure manually")

    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 1)))

(defun meta-log-setup-step-1-llm ()
  "Step 1: Choose LLM backend."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-insert (propertize "\n━━━  Step 1/3: AI Backend  ━━━\n\n" 'face '(:foreground "yellow" :weight bold)))

    (widget-insert "meta-log uses AI to understand natural language queries.\n")
    (widget-insert "Choose how you want to run the AI:\n\n")

    (widget-insert (propertize "Option 1: Local AI (Ollama)\n" 'face '(:foreground "green" :weight bold)))
    (widget-insert "  • Free, private, runs on your device\n")
    (widget-insert "  • Requires: 2GB disk space, 4GB RAM\n")
    (widget-insert "  • Best for: Privacy, offline work\n\n")

    (widget-insert (propertize "Option 2: Cloud AI (OpenAI/Anthropic)\n" 'face '(:foreground "cyan" :weight bold)))
    (widget-insert "  • More powerful, faster responses\n")
    (widget-insert "  • Requires: API key, internet connection\n")
    (widget-insert "  • Best for: Complex queries, large knowledge bases\n\n")

    (widget-insert (propertize "Option 3: No AI (Prolog/Datalog only)\n" 'face '(:foreground "magenta" :weight bold)))
    (widget-insert "  • Pure logic programming, no LLM\n")
    (widget-insert "  • Best for: Structured data, formal queries\n\n")

    (let ((backend-widget
           (widget-create 'radio-button-choice
                          :value (or (alist-get 'llm-backend meta-log-setup-state) 'ollama)
                          :notify (lambda (widget &rest _)
                                    (setf (alist-get 'llm-backend meta-log-setup-state)
                                          (widget-value widget)))
                          '(item :tag "Local AI (Ollama)" :value ollama)
                          '(item :tag "Cloud AI (OpenAI/Anthropic)" :value api)
                          '(item :tag "No AI (Logic only)" :value none))))

      (widget-insert "\n\n")
      (widget-create 'push-button
                     :notify (lambda (&rest _)
                               (meta-log-setup-configure-llm (widget-value backend-widget)))
                     "Next →")

      (widget-insert "  ")
      (widget-create 'push-button
                     :notify (lambda (&rest _) (meta-log-setup-welcome-screen))
                     "← Back")

      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min))
      (widget-forward 1))))

(defun meta-log-setup-configure-llm (backend)
  "Configure the selected LLM BACKEND."
  (setf (alist-get 'llm-backend meta-log-setup-state) backend)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-insert (propertize "\n━━━  Configuring AI Backend  ━━━\n\n" 'face '(:foreground "yellow" :weight bold)))

    (pcase backend
      ('ollama
       (widget-insert "Checking Ollama installation...\n\n")
       (if (executable-find "ollama")
           (progn
             (widget-insert "✓ Ollama found!\n")
             (widget-insert "\nDownloading recommended model (gemma2:2b)...\n")
             (widget-insert "This may take a few minutes...\n\n")
             (let ((proc (start-process "ollama-pull" "*ollama-pull*" "ollama" "pull" "gemma2:2b")))
               (set-process-sentinel
                proc
                (lambda (p _e)
                  (when (eq (process-status p) 'exit)
                    (message "✓ Model downloaded successfully!")
                    (sleep-for 1)
                    (meta-log-setup-step-2-notes))))))
         (widget-insert "❌ Ollama not found.\n\n")
         (widget-insert "Install with:\n")
         (widget-insert "  curl -fsSL https://ollama.com/install.sh | sh\n\n")
         (widget-create 'push-button
                        :notify (lambda (&rest _) (meta-log-setup-step-2-notes))
                        "Skip (configure later)")
         (widget-insert "  ")
         (widget-create 'push-button
                        :notify (lambda (&rest _) (meta-log-setup-step-1-llm))
                        "← Back")))

      ('api
       (widget-insert "Enter your API key:\n\n")
       (widget-insert "Provider: ")
       (let ((provider-widget
              (widget-create 'radio-button-choice
                             :value 'openai
                             '(item :tag "OpenAI" :value openai)
                             '(item :tag "Anthropic" :value anthropic))))
         (widget-insert "\n\nAPI Key: ")
         (let ((api-key-widget (widget-create 'editable-field "")))
           (widget-insert "\n\n")
           (widget-create 'push-button
                          :notify (lambda (&rest _)
                                    (setf (alist-get 'api-provider meta-log-setup-state)
                                          (widget-value provider-widget))
                                    (setf (alist-get 'api-key meta-log-setup-state)
                                          (widget-value api-key-widget))
                                    (meta-log-setup-step-2-notes))
                          "Next →")
           (widget-insert "  ")
           (widget-create 'push-button
                          :notify (lambda (&rest _) (meta-log-setup-step-1-llm))
                          "← Back"))))

      ('none
       (widget-insert "No LLM configured. You can still use:\n")
       (widget-insert "  • Prolog queries\n")
       (widget-insert "  • Datalog queries\n")
       (widget-insert "  • M-expressions\n\n")
       (sleep-for 1)
       (meta-log-setup-step-2-notes)))

    (use-local-map widget-keymap)
    (widget-setup)))

(defun meta-log-setup-step-2-notes ()
  "Step 2: Import notes folder."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-insert (propertize "\n━━━  Step 2/3: Import Your Notes  ━━━\n\n" 'face '(:foreground "yellow" :weight bold)))

    (widget-insert "Point meta-log to your existing notes folder.\n")
    (widget-insert "Supported formats: .md, .org, .txt, .pdf\n\n")

    (widget-insert "Notes folder path:\n")
    (let ((folder-widget
           (widget-create 'editable-field
                          :size 50
                          :value (or (alist-get 'notes-folder meta-log-setup-state)
                                     (expand-file-name "~/Documents")))))

      (widget-insert "  ")
      (widget-create 'push-button
                     :notify (lambda (&rest _)
                               (let ((dir (read-directory-name "Select notes folder: ")))
                                 (widget-value-set folder-widget dir)))
                     "Browse...")

      (widget-insert "\n\n")
      (let ((auto-ingest-widget
             (widget-create 'checkbox
                            :value (alist-get 'auto-ingest meta-log-setup-state t))))
        (widget-insert " Auto-import new files when they're added\n\n")

        (widget-insert (propertize "What will happen:\n" 'face '(:weight bold)))
        (widget-insert "  1. Scan all files in this folder\n")
        (widget-insert "  2. Extract text and metadata\n")
        (widget-insert "  3. Build searchable index\n")
        (widget-insert "  4. Create knowledge graph connections\n\n")

        (widget-create 'push-button
                       :notify (lambda (&rest _)
                                 (let ((folder (widget-value folder-widget)))
                                   (if (file-directory-p folder)
                                       (progn
                                         (setf (alist-get 'notes-folder meta-log-setup-state) folder)
                                         (setf (alist-get 'auto-ingest meta-log-setup-state)
                                               (widget-value auto-ingest-widget))
                                         (meta-log-setup-step-3-finish))
                                     (message "❌ Directory does not exist: %s" folder))))
                       "Import & Continue →")

        (widget-insert "  ")
        (widget-create 'push-button
                       :notify (lambda (&rest _)
                                 (setf (alist-get 'notes-folder meta-log-setup-state) nil)
                                 (meta-log-setup-step-3-finish))
                       "Skip (no import)")

        (widget-insert "  ")
        (widget-create 'push-button
                       :notify (lambda (&rest _) (meta-log-setup-step-1-llm))
                       "← Back")

        (use-local-map widget-keymap)
        (widget-setup)
        (goto-char (point-min))
        (widget-forward 1)))))

(defun meta-log-setup-step-3-finish ()
  "Step 3: Finalize setup and save configuration."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-insert (propertize "\n━━━  Step 3/3: Building Your Knowledge System  ━━━\n\n"
                               'face '(:foreground "yellow" :weight bold)))

    (widget-insert "Processing...\n\n")

    ;; Save configuration
    (meta-log-setup-save-config)

    ;; Initialize meta-log
    (require 'meta-log)
    (meta-log-initialize)

    ;; Import notes if specified
    (when-let ((folder (alist-get 'notes-folder meta-log-setup-state)))
      (widget-insert (format "📁 Importing notes from: %s\n" folder))
      ;; This will be implemented in meta-log-ingest.el
      (when (fboundp 'meta-log-ingest-folder)
        (meta-log-ingest-folder folder)))

    (widget-insert "\n")
    (widget-insert (propertize "✓ Setup Complete!\n\n" 'face '(:foreground "green" :weight bold :height 1.5)))

    (widget-insert "Your configuration:\n")
    (widget-insert (format "  • AI Backend: %s\n" (alist-get 'llm-backend meta-log-setup-state)))
    (when-let ((folder (alist-get 'notes-folder meta-log-setup-state)))
      (widget-insert (format "  • Notes Folder: %s\n" folder)))
    (widget-insert (format "  • Config saved to: %s\n\n" meta-log-setup-config-file))

    (widget-insert (propertize "Try these commands:\n" 'face '(:weight bold)))
    (widget-insert "  M-x meta-log-chat     → Ask questions in natural language\n")
    (widget-insert "  M-x meta-log-dashboard → View your knowledge graph\n")
    (widget-insert "  M-x meta-log-ask      → Quick query\n\n")

    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (kill-buffer meta-log-setup-buffer)
                             (meta-log-dashboard))
                   "Open Dashboard")

    (widget-insert "  ")
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (kill-buffer meta-log-setup-buffer)
                             (meta-log-chat))
                   "Start Chatting")

    (widget-insert "  ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (kill-buffer meta-log-setup-buffer))
                   "Close")

    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 1)))

(defun meta-log-setup-save-config ()
  "Save setup configuration to file."
  (let ((config-dir (file-name-directory meta-log-setup-config-file)))
    (unless (file-directory-p config-dir)
      (make-directory config-dir t))

    (with-temp-file meta-log-setup-config-file
      (insert ";;; meta-log configuration (auto-generated)\n\n")
      (insert (format "(setq meta-log-llm-backend '%s)\n"
                      (alist-get 'llm-backend meta-log-setup-state)))

      (when-let ((folder (alist-get 'notes-folder meta-log-setup-state)))
        (insert (format "(setq meta-log-notes-folder \"%s\")\n" folder))
        (insert (format "(setq meta-log-auto-ingest %s)\n"
                        (alist-get 'auto-ingest meta-log-setup-state))))

      (when-let ((provider (alist-get 'api-provider meta-log-setup-state)))
        (insert (format "(setq meta-log-api-provider '%s)\n" provider)))

      (when-let ((key (alist-get 'api-key meta-log-setup-state)))
        (insert (format "(setq meta-log-api-key \"%s\")\n" key)))

      (insert "\n(provide 'meta-log-config)\n"))))

;; Auto-run setup on first launch if no config exists
(unless (file-exists-p meta-log-setup-config-file)
  (add-hook 'emacs-startup-hook 'meta-log-setup))

(provide 'meta-log-setup)

;;; meta-log-setup.el ends here
