;;; run-all-tests.el --- Test runner for meta-log

;; Master test runner that executes all test suites

;; Add load paths
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (file-name-directory (or load-file-name default-directory)))

(require 'cl-lib)

;; Load test suites
(load (expand-file-name "test-core.el" (file-name-directory (or load-file-name default-directory))))
(load (expand-file-name "test-llm.el" (file-name-directory (or load-file-name default-directory))))
(load (expand-file-name "test-federation.el" (file-name-directory (or load-file-name default-directory))))
(load (expand-file-name "e2e/test-workflow.el" (file-name-directory (or load-file-name default-directory))))

(defun run-all-tests ()
  "Run all test suites and report results."
  (interactive)
  (message "")
  (message "╔════════════════════════════════════════════╗")
  (message "║     meta-log Complete Test Suite          ║")
  (message "╚════════════════════════════════════════════╝")
  (message "")

  (let ((start-time (current-time))
        (results '()))

    ;; Run all test suites
    (message "📦 Running Unit Tests...")
    (message "")
    (push (cons "Core Tests" (test-core-all)) results)
    (message "")
    (push (cons "LLM Tests" (test-llm-all)) results)
    (message "")
    (push (cons "Federation Tests" (test-federation-all)) results)
    (message "")

    (message "🔄 Running E2E Tests...")
    (message "")
    (push (cons "E2E Workflow Tests" (test-e2e-all)) results)
    (message "")

    ;; Calculate statistics
    (let* ((total (length results))
           (passed (length (cl-remove-if-not #'cdr results)))
           (failed (- total passed))
           (elapsed (float-time (time-subtract (current-time) start-time))))

      (message "")
      (message "╔════════════════════════════════════════════╗")
      (message "║           Final Test Results               ║")
      (message "╚════════════════════════════════════════════╝")
      (message "")

      ;; Show individual suite results
      (dolist (result (reverse results))
        (let ((suite-name (car result))
              (suite-passed (cdr result)))
          (message "%s %s"
                   (if suite-passed "✓" "✗")
                   suite-name)))

      (message "")
      (message "Total Suites: %d" total)
      (message "Passed: %d" passed)
      (message "Failed: %d" failed)
      (message "Time: %.2fs" elapsed)
      (message "")

      (if (= passed total)
          (progn
            (message "🎉 All test suites passed!")
            (message "")
            0)
        (progn
          (message "❌ Some test suites failed")
          (message "")
          1)))))

;; Run tests if executed directly
(when noninteractive
  (let ((exit-code (run-all-tests)))
    (kill-emacs exit-code)))

(provide 'run-all-tests)
;;; run-all-tests.el ends here
