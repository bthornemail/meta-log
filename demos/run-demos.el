;;; run-demos.el --- Interactive demo launcher for meta-log

;; Interactive menu system for running meta-log demos

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'meta-log)

(defvar demo-list
  '(("Personal Knowledge Base" . "01-personal-knowledge-base.el")
    ("Research Assistant" . "02-research-assistant.el")
    ("Code Analysis" . "03-code-analysis.el")
    ("Team Collaboration" . "04-team-collaboration.el")
    ("Mathematical Modules" . "05-mathematical-modules.el"))
  "List of available demos.")

(defun demo-launcher-menu ()
  "Display interactive demo launcher menu."
  (interactive)
  (let ((buffer (get-buffer-create "*meta-log Demos*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "╔════════════════════════════════════════════════════════════╗\n")
      (insert "║                                                            ║\n")
      (insert "║              meta-log Interactive Demos                    ║\n")
      (insert "║                                                            ║\n")
      (insert "╚════════════════════════════════════════════════════════════╝\n\n")

      (insert "Welcome to meta-log! Choose a demo to explore:\n\n")

      (let ((num 1))
        (dolist (demo demo-list)
          (insert (format "[%d] %s\n" num (car demo)))
          (setq num (1+ num))))

      (insert "\n[a] Run All Demos\n")
      (insert "[q] Quit\n\n")

      (insert "═══════════════════════════════════════════════════════════\n\n")
      (insert "Demo Descriptions:\n\n")

      (insert "1. Personal Knowledge Base\n")
      (insert "   Build and query your personal knowledge from notes.\n")
      (insert "   Perfect for students, researchers, and writers.\n\n")

      (insert "2. Research Assistant\n")
      (insert "   AI-powered assistant that learns from interactions.\n")
      (insert "   Ideal for academic research and continuous learning.\n\n")

      (insert "3. Code Analysis\n")
      (insert "   Analyze code structure and generate documentation.\n")
      (insert "   Great for developers and technical leads.\n\n")

      (insert "4. Team Collaboration\n")
      (insert "   Federated knowledge sharing across team members.\n")
      (insert "   Essential for distributed teams and open source.\n\n")

      (insert "5. Mathematical Modules\n")
      (insert "   Quadratic forms, quaternions, p-adic arithmetic.\n")
      (insert "   Geometric alignments and Drinfeld modules.\n\n")

      (insert "═══════════════════════════════════════════════════════════\n\n")
      (insert "Enter your choice: ")

      (goto-char (point-max)))

    (switch-to-buffer buffer)
    (demo-launcher-read-choice)))

(defun demo-launcher-read-choice ()
  "Read user's demo choice and launch it."
  (let ((choice (read-char-exclusive)))
    (cond
     ;; Individual demos
     ((and (>= choice ?1) (<= choice ?5))
      (let* ((index (- choice ?1))
             (demo (nth index demo-list))
             (demo-file (cdr demo)))
        (message "Loading %s..." (car demo))
        (demo-launcher-run-demo demo-file)))

     ;; Run all demos
     ((= choice ?a)
      (demo-launcher-run-all))

     ;; Quit
     ((= choice ?q)
      (message "Thanks for exploring meta-log!")
      (kill-buffer "*meta-log Demos*"))

     ;; Invalid choice
     (t
      (message "Invalid choice. Press any key to try again.")
      (read-char-exclusive)
      (demo-launcher-menu)))))

(defun demo-launcher-run-demo (demo-file)
  "Load and run a specific demo file."
  (let ((demo-path (expand-file-name demo-file
                                     (file-name-directory (or load-file-name
                                                              default-directory)))))
    (if (file-exists-p demo-path)
        (progn
          (load-file demo-path)
          ;; Call the demo's main function
          (cond
           ((string-match "01-personal" demo-file)
            (demo-personal-kb-full))
           ((string-match "02-research" demo-file)
            (demo-research-full))
           ((string-match "03-code" demo-file)
            (demo-code-full))
           ((string-match "04-team" demo-file)
            (demo-collab-full))
           ((string-match "05-mathematical" demo-file)
            (demo-math-full)))

          ;; Show completion message
          (message "")
          (message "Demo complete! Press 'm' for menu, 'q' to quit")
          (let ((next-choice (read-char-exclusive)))
            (cond
             ((= next-choice ?m)
              (demo-launcher-menu))
             ((= next-choice ?q)
              (message "Thanks for exploring meta-log!")))))
      (error "Demo file not found: %s" demo-path))))

(defun demo-launcher-run-all ()
  "Run all demos sequentially."
  (message "Running all demos...")
  (message "This will take a few minutes. Press C-g to cancel.")
  (sit-for 2)

  (dolist (demo demo-list)
    (message "")
    (message "═══════════════════════════════════════════════════════════")
    (message "Starting: %s" (car demo))
    (message "═══════════════════════════════════════════════════════════")
    (sit-for 1)

    (demo-launcher-run-demo (cdr demo))

    (message "")
    (message "Press any key to continue to next demo...")
    (read-char-exclusive))

  (message "")
  (message "╔════════════════════════════════════════════════════════════╗")
  (message "║         All Demos Complete!                                ║")
  (message "╚════════════════════════════════════════════════════════════╝")
  (message "")
  (message "You've explored all meta-log capabilities!")
  (message "Ready to use meta-log for your own projects?")
  (message "")
  (message "Next steps:")
  (message "  M-x meta-log-dashboard    - Start the main interface")
  (message "  M-x meta-log-ingest-directory - Import your notes")
  (message "  M-x meta-log-chat         - Start a chat session")
  (message ""))

(defun demo-quick-start ()
  "Quick start guide for first-time users."
  (interactive)
  (let ((buffer (get-buffer-create "*meta-log Quick Start*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "╔════════════════════════════════════════════════════════════╗\n")
      (insert "║                                                            ║\n")
      (insert "║              meta-log Quick Start Guide                    ║\n")
      (insert "║                                                            ║\n")
      (insert "╚════════════════════════════════════════════════════════════╝\n\n")

      (insert "👋 Welcome to meta-log!\n\n")

      (insert "meta-log is an AI-powered knowledge system that helps you:\n")
      (insert "  • Organize and query your notes with natural language\n")
      (insert "  • Learn and adapt from your interactions\n")
      (insert "  • Analyze code and generate documentation\n")
      (insert "  • Share knowledge across teams\n\n")

      (insert "═══════════════════════════════════════════════════════════\n\n")

      (insert "🚀 Getting Started (Choose One):\n\n")

      (insert "[1] Run Interactive Demos\n")
      (insert "    Explore what meta-log can do with hands-on examples\n\n")

      (insert "[2] Import My Notes\n")
      (insert "    Start using meta-log with your existing notes\n\n")

      (insert "[3] Start Fresh\n")
      (insert "    Open the dashboard and begin exploring\n\n")

      (insert "═══════════════════════════════════════════════════════════\n\n")

      (insert "Enter your choice (1-3): ")

      (goto-char (point-max)))

    (switch-to-buffer buffer)
    (let ((choice (read-char-exclusive)))
      (cond
       ((= choice ?1)
        (kill-buffer "*meta-log Quick Start*")
        (demo-launcher-menu))
       ((= choice ?2)
        (kill-buffer "*meta-log Quick Start*")
        (call-interactively 'meta-log-ingest-directory))
       ((= choice ?3)
        (kill-buffer "*meta-log Quick Start*")
        (meta-log-dashboard))
       (t
        (message "Invalid choice")
        (demo-quick-start))))))

;; Auto-launch menu when file is loaded interactively
(when (called-interactively-p 'any)
  (demo-launcher-menu))

(provide 'run-demos)
;;; run-demos.el ends here
