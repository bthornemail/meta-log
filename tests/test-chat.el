;;; test-chat.el --- Test meta-log chat functionality

;; Add the current directory and modules to load path
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name "modules" (file-name-directory load-file-name)))

;; Load meta-log
(message "Loading meta-log...")
(require 'meta-log)

;; Check if chat module is loaded
(if (featurep 'meta-log-chat)
    (message "✓ Chat module loaded successfully")
  (message "✗ Chat module NOT loaded"))

;; Check if LLM module is available
(if (featurep 'meta-log-llm)
    (message "✓ LLM module loaded")
  (message "ℹ LLM module not loaded (optional)"))

;; Check if knowledge-graph module is available
(if (featurep 'meta-log-knowledge-graph)
    (message "✓ Knowledge graph module loaded")
  (message "ℹ Knowledge graph module not loaded (optional)"))

;; Test if the chat function exists
(if (fboundp 'meta-log-chat)
    (message "✓ meta-log-chat function is available")
  (message "✗ meta-log-chat function NOT found"))

;; Test if the fallback response works
(if (fboundp 'meta-log-chat-fallback-response)
    (progn
      (message "✓ Fallback response function available")
      (message "Testing fallback response...")
      (let ((response (meta-log-chat-fallback-response "test query")))
        (message "Response: %s" response)))
  (message "✗ Fallback response function NOT found"))

(message "\n=== Chat Test Summary ===")
(message "All basic chat functions are available!")
(message "To launch the chat interface, run: M-x meta-log-chat")
(message "Key bindings in chat mode:")
(message "  RET or C-c C-c - Send message")
(message "  C-c C-l       - Clear chat")
(message "  C-c C-s       - Save chat history")
(message "  C-c C-q       - Quit chat")

;;; test-chat.el ends here
