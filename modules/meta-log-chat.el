;;; meta-log-chat.el --- ChatGPT-style interface for meta-log

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne <bthornemail@gmail.com>

;;; Commentary:
;; Simple chat interface that feels like ChatGPT/Claude
;; User types question → meta-log answers using LLM + knowledge graph
;; The primary way most users will interact with meta-log

;;; Code:

(require 'meta-log-llm nil t)
(require 'meta-log-knowledge-graph nil t)

(defvar meta-log-chat-buffer "*meta-log Chat*")
(defvar meta-log-chat-history nil
  "Chat conversation history.")

(defface meta-log-chat-user-face
  '((t :foreground "cyan" :weight bold))
  "Face for user messages in chat.")

(defface meta-log-chat-assistant-face
  '((t :foreground "green"))
  "Face for assistant messages in chat.")

(defface meta-log-chat-system-face
  '((t :foreground "yellow" :slant italic))
  "Face for system messages in chat.")

;;;###autoload
(defun meta-log-chat ()
  "Launch the meta-log chat interface - primary user interaction mode."
  (interactive)
  (let ((buf (get-buffer-create meta-log-chat-buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'meta-log-chat-mode)
        (meta-log-chat-mode))
      (when (= (buffer-size) 0)
        (meta-log-chat-insert-welcome)))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(defun meta-log-chat-insert-welcome ()
  "Insert welcome message in chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "
╔════════════════════════════════════════════════════════════════════╗
║                                                                    ║
║            💬  meta-log Chat  —  Ask Me Anything!                 ║
║                                                                    ║
╚════════════════════════════════════════════════════════════════════╝

" 'face 'meta-log-chat-system-face))

    (insert (propertize "I can help you:\n" 'face 'meta-log-chat-system-face))
    (insert (propertize "  • Search your notes and files\n" 'face 'meta-log-chat-system-face))
    (insert (propertize "  • Answer questions using your knowledge graph\n" 'face 'meta-log-chat-system-face))
    (insert (propertize "  • Execute Prolog/Datalog queries\n" 'face 'meta-log-chat-system-face))
    (insert (propertize "  • Analyze connections between ideas\n\n" 'face 'meta-log-chat-system-face))

    (insert (propertize "Try asking:\n" 'face 'meta-log-chat-system-face))
    (insert (propertize "  • \"What files mention machine learning?\"\n" 'face 'meta-log-chat-system-face))
    (insert (propertize "  • \"Show me notes about Prolog\"\n" 'face 'meta-log-chat-system-face))
    (insert (propertize "  • \"What are the main topics in my notes?\"\n\n" 'face 'meta-log-chat-system-face))

    (insert (propertize "Type your question below and press Enter.\n" 'face 'meta-log-chat-system-face))
    (insert (propertize (make-string 70 ?─) 'face 'meta-log-chat-system-face))
    (insert "\n\n")))

(defun meta-log-chat-send ()
  "Send the current message and get a response."
  (interactive)
  (let* ((inhibit-read-only t)
         (input-start (save-excursion
                        (goto-char (point-max))
                        (forward-line -1)
                        (when (looking-at "You: ")
                          (match-end 0))))
         (query (when input-start
                  (buffer-substring-no-properties input-start (point-max)))))

    (when (and query (not (string-blank-p query)))
      ;; Clean up input
      (goto-char (point-max))
      (insert "\n\n")

      ;; Show typing indicator
      (let ((typing-pos (point)))
        (insert (propertize "meta-log: thinking..." 'face 'meta-log-chat-system-face))
        (redisplay)

        ;; Get response
        (let ((response (meta-log-chat-get-response query)))
          ;; Remove typing indicator
          (delete-region typing-pos (point-max))

          ;; Insert response
          (insert (propertize "meta-log: " 'face 'meta-log-chat-assistant-face))
          (insert (propertize response 'face 'meta-log-chat-assistant-face))
          (insert "\n\n")
          (insert (propertize (make-string 70 ?─) 'face 'meta-log-chat-system-face))
          (insert "\n\n")))

      ;; Prompt for next message
      (insert (propertize "You: " 'face 'meta-log-chat-user-face))
      (goto-char (point-max)))))

(defun meta-log-chat-get-response (query)
  "Get response for QUERY using LLM + knowledge graph."
  (condition-case err
      (progn
        ;; Add to history
        (push query meta-log-chat-history)

        ;; Try to use LLM if available
        (if (and (fboundp 'meta-log-llm-query)
                 (or (boundp 'meta-log-llm-backend)
                     (getenv "OPENAI_API_KEY")
                     (getenv "ANTHROPIC_API_KEY")))

            ;; Use LLM with knowledge graph context
            (let* ((context (meta-log-chat-get-context query))
                   (prompt (meta-log-chat-build-prompt query context)))
              (meta-log-llm-query prompt))

          ;; Fall back to simple keyword search
          (meta-log-chat-fallback-response query)))

    (error
     (format "Sorry, I encountered an error: %s\n\nTry rephrasing your question or check your configuration."
             (error-message-string err)))))

(defun meta-log-chat-get-context (query)
  "Get relevant context from knowledge graph for QUERY."
  (when (fboundp 'meta-log-kg-search)
    (let ((results (meta-log-kg-search query)))
      (mapconcat
       (lambda (node)
         (format "- %s: %s"
                 (alist-get 'title (cdr node))
                 (truncate-string-to-width
                  (or (alist-get 'content (cdr node)) "")
                  200 nil nil "...")))
       (seq-take results 5)
       "\n"))))

(defun meta-log-chat-build-prompt (query context)
  "Build LLM prompt from QUERY and CONTEXT."
  (format "You are a helpful assistant with access to the user's knowledge base.

Here is relevant context from their notes:

%s

User question: %s

Provide a helpful, concise answer based on the context above. If the context doesn't contain relevant information, say so."
          (or context "No relevant notes found.")
          query))

(defun meta-log-chat-fallback-response (query)
  "Generate fallback response for QUERY when no LLM available."
  ;; Simple keyword search through indexed files
  (if (and (boundp 'meta-log-notes-folder)
           meta-log-notes-folder
           (file-directory-p meta-log-notes-folder))

      (let* ((keywords (split-string (downcase query) "\\W+" t))
             (results '()))

        ;; Search for files containing keywords
        (dolist (file (directory-files-recursively
                       meta-log-notes-folder
                       "\\.\\(md\\|org\\|txt\\)$"))
          (with-temp-buffer
            (insert-file-contents file)
            (let ((content (downcase (buffer-string))))
              (when (seq-some (lambda (kw) (string-match-p kw content)) keywords)
                (push (file-name-nondirectory file) results)))))

        (if results
            (format "I found these files that might be relevant:\n\n%s\n\nNote: I don't have an LLM configured, so I can't provide detailed answers. Set up AI in Settings for smarter responses!"
                    (mapconcat (lambda (f) (concat "  • " f))
                               (seq-take results 10)
                               "\n"))

          "I couldn't find any relevant notes. Try:\n  • Checking if your notes are imported (M-x meta-log-ingest-folder)\n  • Using different keywords\n  • Setting up an LLM for smarter search"))

    "No notes folder configured yet! Run M-x meta-log-setup to get started."))

(defun meta-log-chat-clear ()
  "Clear the chat buffer and start fresh."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq meta-log-chat-history nil)
    (meta-log-chat-insert-welcome)
    (insert (propertize "You: " 'face 'meta-log-chat-user-face))
    (goto-char (point-max))))

(defun meta-log-chat-save ()
  "Save chat history to a file."
  (interactive)
  (let ((file (read-file-name "Save chat to: "
                              (when (boundp 'meta-log-notes-folder)
                                meta-log-notes-folder)
                              nil nil
                              (format "chat-%s.md"
                                      (format-time-string "%Y%m%d-%H%M%S")))))
    (write-region (point-min) (point-max) file)
    (message "Chat saved to %s" file)))

(defvar meta-log-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'meta-log-chat-send)
    (define-key map (kbd "C-c C-c") 'meta-log-chat-send)
    (define-key map (kbd "C-c C-l") 'meta-log-chat-clear)
    (define-key map (kbd "C-c C-s") 'meta-log-chat-save)
    (define-key map (kbd "C-c C-q") 'quit-window)
    map)
  "Keymap for meta-log-chat-mode.")

(define-derived-mode meta-log-chat-mode fundamental-mode "meta-log Chat"
  "Major mode for chatting with meta-log.

\\{meta-log-chat-mode-map}"
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)

  ;; Make most of buffer read-only, except input area
  (add-hook 'after-change-functions 'meta-log-chat-protect-history nil t))

(defun meta-log-chat-protect-history (beg end _len)
  "Protect chat history from editing, only allow editing current input.
BEG and END are the change boundaries."
  ;; Allow changes only after the last \"You: \" prompt
  (let ((last-prompt (save-excursion
                       (goto-char (point-max))
                       (when (search-backward "You: " nil t)
                         (match-end 0)))))
    (when (and last-prompt (< beg last-prompt))
      ;; Trying to edit history - undo it
      (undo)
      (message "Chat history is read-only. Type new messages at the bottom."))))

(provide 'meta-log-chat)

;;; meta-log-chat.el ends here
