;;; test-llm.el --- LLM integration test suite

;; Test suite for meta-log LLM functionality

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log)
(require 'meta-log-llm)

(defun test-llm-initialization ()
  "Test LLM initialization."
  (message "Testing LLM initialization...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test LLM initialization
          (meta-log-llm-initialize)

          ;; Check if main query function exists
          (unless (fboundp 'meta-log-llm-query)
            (push "LLM query function should exist" errors)))
      (error (push (format "LLM init error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ LLM initialization tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ LLM initialization tests passed")
        t))))

(defun test-llm-vocabulary ()
  "Test LLM vocabulary management."
  (message "Testing LLM vocabulary...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test vocabulary addition
          (meta-log-llm-add-vocabulary "machine learning" "ml")
          (meta-log-llm-add-vocabulary "artificial intelligence" "ai")

          ;; Test vocabulary application
          (let ((result (meta-log-llm-apply-vocabulary "what is machine learning?")))
            (unless result
              (push "Vocabulary application should work" errors))))
      (error (push (format "Vocabulary error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ LLM vocabulary tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ LLM vocabulary tests passed")
        t))))

(defun test-llm-translation ()
  "Test LLM translation to logic queries."
  (message "Testing LLM translation...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Check translation functions exist
          (unless (fboundp 'meta-log-llm-to-prolog)
            (push "Prolog translation function should exist" errors))

          (unless (fboundp 'meta-log-llm-to-datalog)
            (push "Datalog translation function should exist" errors)))
      (error (push (format "Translation error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ LLM translation tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ LLM translation tests passed")
        t))))

(defun test-llm-provider-config ()
  "Test LLM provider configuration."
  (message "Testing LLM provider configuration...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test backend variable exists
          (unless (boundp 'meta-log-llm-backend)
            (push "Backend variable should exist" errors))

          ;; Test backend check function
          (unless (fboundp 'meta-log-llm-backend-available-p)
            (push "Backend check function should exist" errors)))
      (error (push (format "Provider config error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ LLM provider config tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ LLM provider config tests passed")
        t))))

(defun test-llm-stats ()
  "Test LLM statistics function."
  (message "Testing LLM stats...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test stats function
          (when (fboundp 'meta-log-llm-stats)
            (meta-log-llm-stats)))
      (error (push (format "Stats error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ LLM stats tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ LLM stats tests passed")
        t))))

(defun test-llm-all ()
  "Run all LLM tests."
  (message "=== Running LLM Test Suite ===")
  (let ((results '()))
    (push (test-llm-initialization) results)
    (push (test-llm-vocabulary) results)
    (push (test-llm-translation) results)
    (push (test-llm-provider-config) results)
    (push (test-llm-stats) results)

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
