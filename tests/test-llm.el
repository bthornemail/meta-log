;;; test-llm.el --- LLM integration test suite

;; Test suite for meta-log LLM functionality

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log)
(require 'meta-log-llm)

(defun test-llm-cache ()
  "Test LLM caching functionality."
  (message "Testing LLM cache...")
  (let ((errors '())
        (test-key "test-prompt")
        (test-value "test response"))
    (condition-case err
        (progn
          ;; Test cache write
          (meta-log-llm-cache-put test-key test-value)

          ;; Test cache read
          (let ((cached (meta-log-llm-cache-get test-key)))
            (unless cached
              (push "Should retrieve cached value" errors))
            (unless (equal cached test-value)
              (push "Cached value should match" errors)))

          ;; Test cache invalidation
          (meta-log-llm-cache-invalidate test-key)
          (let ((cached (meta-log-llm-cache-get test-key)))
            (when cached
              (push "Cache should be invalidated" errors))))
      (error (push (format "Cache error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ LLM cache tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ LLM cache tests passed")
        t))))

(defun test-llm-learning ()
  "Test LLM learning functionality."
  (message "Testing LLM learning...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test learning record creation
          (meta-log-llm-record-interaction
           "What is Prolog?"
           "Prolog is a logic programming language.")

          ;; Test learning retrieval
          (let ((learnings (meta-log-llm-get-learnings "Prolog")))
            (unless learnings
              (push "Should have recorded learning" errors))))
      (error (push (format "Learning error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ LLM learning tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ LLM learning tests passed")
        t))))

(defun test-llm-provider-config ()
  "Test LLM provider configuration."
  (message "Testing LLM provider configuration...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test provider setting
          (setq meta-log-llm-provider 'anthropic)
          (unless (equal meta-log-llm-provider 'anthropic)
            (push "Should set provider" errors))

          ;; Test provider switching
          (setq meta-log-llm-provider 'openai)
          (unless (equal meta-log-llm-provider 'openai)
            (push "Should switch provider" errors)))
      (error (push (format "Provider config error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ LLM provider config tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ LLM provider config tests passed")
        t))))

(defun test-llm-all ()
  "Run all LLM tests."
  (message "=== Running LLM Test Suite ===")
  (let ((results '()))
    (push (test-llm-cache) results)
    (push (test-llm-learning) results)
    (push (test-llm-provider-config) results)

    (let ((passed (length (cl-remove-if-not 'identity results)))
          (total (length results)))
      (message "")
      (message "=== Test Results ===")
      (message "Passed: %d/%d" passed total)
      (if (= passed total)
          (progn
            (message "✓ All tests passed!")
            t)
        (progn
          (message "✗ Some tests failed")
          nil)))))

;; Run tests if executed directly
(when (and (not noninteractive) (equal (buffer-name) "*scratch*"))
  (test-llm-all))

(provide 'test-llm)
;;; test-llm.el ends here
