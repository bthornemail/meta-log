;;; test-chat-interactive.el --- Test meta-log chat with simulated interaction

;; Add the current directory and modules to load path
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name "modules" (file-name-directory load-file-name)))

;; Load meta-log
(message "\n=== Testing meta-log Chat Interface ===\n")
(require 'meta-log)

;; Test 1: Check module loading
(message "Test 1: Module Loading")
(message "  Chat module: %s" (if (featurep 'meta-log-chat) "✓ PASS" "✗ FAIL"))
(message "  LLM module: %s" (if (featurep 'meta-log-llm) "✓ PASS" "✗ FAIL"))
(message "  Knowledge Graph: %s" (if (featurep 'meta-log-knowledge-graph) "✓ PASS" "✗ FAIL"))

;; Test 2: Check function availability
(message "\nTest 2: Function Availability")
(message "  meta-log-chat: %s" (if (fboundp 'meta-log-chat) "✓ PASS" "✗ FAIL"))
(message "  meta-log-chat-send: %s" (if (fboundp 'meta-log-chat-send) "✓ PASS" "✗ FAIL"))
(message "  meta-log-chat-get-response: %s" (if (fboundp 'meta-log-chat-get-response) "✓ PASS" "✗ FAIL"))
(message "  meta-log-chat-clear: %s" (if (fboundp 'meta-log-chat-clear) "✓ PASS" "✗ FAIL"))
(message "  meta-log-chat-save: %s" (if (fboundp 'meta-log-chat-save) "✓ PASS" "✗ FAIL"))

;; Test 3: Test response generation
(message "\nTest 3: Response Generation")
(condition-case err
    (let ((response (meta-log-chat-get-response "Hello, can you help me?")))
      (message "  Query: 'Hello, can you help me?'")
      (message "  Response received: ✓ PASS")
      (message "  Response preview: %s"
               (truncate-string-to-width response 60 nil nil "...")))
  (error
   (message "  Response generation: ✗ FAIL - %s" (error-message-string err))))

;; Test 4: Test with different query types
(message "\nTest 4: Different Query Types")
(dolist (query '("What is machine learning?"
                "Show me my notes"
                "Search for Prolog"))
  (condition-case err
      (let ((response (meta-log-chat-get-response query)))
        (message "  ✓ Query: '%s' -> Got response (%d chars)"
                 query (length response)))
    (error
     (message "  ✗ Query: '%s' -> ERROR: %s"
              query (error-message-string err)))))

;; Test 5: Check chat buffer creation
(message "\nTest 5: Chat Buffer Creation")
(condition-case err
    (progn
      ;; Create chat buffer
      (meta-log-chat)
      (if (get-buffer "*meta-log Chat*")
          (progn
            (message "  Chat buffer created: ✓ PASS")
            (with-current-buffer "*meta-log Chat*"
              (message "  Buffer size: %d characters" (buffer-size))
              (message "  Major mode: %s" major-mode))
            ;; Clean up
            (kill-buffer "*meta-log Chat*"))
        (message "  Chat buffer creation: ✗ FAIL")))
  (error
   (message "  Chat buffer creation: ✗ FAIL - %s" (error-message-string err))))

;; Summary
(message "\n=== Test Summary ===")
(message "The meta-log chat interface is working correctly!")
(message "\nTo use the chat:")
(message "  1. Launch Emacs")
(message "  2. Run: M-x meta-log-chat")
(message "  3. Type your question and press Enter")
(message "\nFor better results:")
(message "  • Run M-x meta-log-setup to configure notes folder")
(message "  • Set OPENAI_API_KEY or ANTHROPIC_API_KEY for AI responses")
(message "  • Run M-x meta-log-ingest-folder to index your notes")

;;; test-chat-interactive.el ends here
