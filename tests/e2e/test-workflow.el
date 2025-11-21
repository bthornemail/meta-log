;;; test-workflow.el --- End-to-end workflow test suite

;; E2E test suite for meta-log complete workflows

;; Add load path
(add-to-list 'load-path (expand-file-name "../../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log)
(require 'meta-log-knowledge-graph)
(require 'meta-log-llm)

(defvar test-e2e-temp-dir nil
  "Temporary directory for e2e tests.")

(defun test-e2e-setup ()
  "Set up e2e test environment."
  (setq test-e2e-temp-dir (make-temp-file "meta-log-e2e-" t))
  (message "✓ E2E test environment created: %s" test-e2e-temp-dir))

(defun test-e2e-teardown ()
  "Clean up e2e test environment."
  (when (and test-e2e-temp-dir (file-exists-p test-e2e-temp-dir))
    (delete-directory test-e2e-temp-dir t)
    (message "✓ E2E test environment cleaned up")))

(defun test-e2e-knowledge-ingestion ()
  "Test complete knowledge ingestion workflow."
  (message "Testing knowledge ingestion workflow...")
  (let ((errors '())
        (test-org-file (expand-file-name "notes.org" test-e2e-temp-dir)))
    (condition-case err
        (progn
          ;; Create test org file
          (with-temp-file test-org-file
            (insert "#+TITLE: Test Knowledge Base\n\n")
            (insert "* Machine Learning\n")
            (insert "Machine learning is a subset of AI.\n\n")
            (insert "** Neural Networks\n")
            (insert "Neural networks are computational models.\n\n")
            (insert "* Programming Languages\n")
            (insert "** Lisp\n")
            (insert "Lisp is a family of programming languages.\n"))

          ;; Initialize system
          (meta-log-initialize)

          ;; Ingest the file
          (meta-log-kg-ingest-file test-org-file)

          ;; Verify stats function works
          (when (fboundp 'meta-log-kg-stats)
            (meta-log-kg-stats)))
      (error (push (format "Ingestion workflow error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Knowledge ingestion workflow failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Knowledge ingestion workflow passed")
        t))))

(defun test-e2e-query-workflow ()
  "Test complete query workflow from natural language to results."
  (message "Testing query workflow...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Initialize system
          (unless meta-log--initialized-p
            (meta-log-initialize))

          ;; Add test knowledge
          (meta-log-prolog-add-fact 'programming_language 'lisp)
          (meta-log-prolog-add-fact 'programming_language 'python)
          (meta-log-prolog-add-fact 'paradigm 'lisp 'functional)
          (meta-log-prolog-add-fact 'paradigm 'python 'imperative)

          ;; Test Prolog query
          (let ((result (meta-log-prolog-query '(programming_language ?X))))
            (unless result
              (push "Prolog query should return results" errors)))

          ;; Test M-expression query
          (let ((result (meta-log-m-expr-parse "[query [programming_language X]]")))
            (unless result
              (push "M-expression should parse" errors))))
      (error (push (format "Query workflow error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Query workflow failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Query workflow passed")
        t))))

(defun test-e2e-llm-workflow ()
  "Test LLM integration workflow."
  (message "Testing LLM workflow...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Initialize system
          (unless meta-log--initialized-p
            (meta-log-initialize))

          ;; Initialize LLM
          (meta-log-llm-initialize)

          ;; Test vocabulary management
          (meta-log-llm-add-vocabulary "test term" "canonical-test")

          ;; Test translation functions exist
          (unless (fboundp 'meta-log-llm-to-prolog)
            (push "LLM to Prolog translation should exist" errors)))
      (error (push (format "LLM workflow error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ LLM workflow failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ LLM workflow passed")
        t))))

(defun test-e2e-integration ()
  "Test integration between different components."
  (message "Testing component integration...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Initialize system
          (unless meta-log--initialized-p
            (meta-log-initialize))

          ;; Test Prolog to Datalog integration
          (meta-log-prolog-add-fact 'test 'integration 'works)
          (let ((result (meta-log-prolog-query '(test integration ?X))))
            (unless result
              (push "Prolog integration query failed" errors)))

          ;; Test KG query function exists
          (unless (fboundp 'meta-log-kg-query)
            (push "KG query function should exist" errors)))
      (error (push (format "Integration error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Integration tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Integration tests passed")
        t))))

(defun test-e2e-all ()
  "Run all e2e workflow tests."
  (message "=== Running E2E Test Suite ===")
  (test-e2e-setup)
  (let ((results '()))
    (push (test-e2e-knowledge-ingestion) results)
    (push (test-e2e-query-workflow) results)
    (push (test-e2e-llm-workflow) results)
    (push (test-e2e-integration) results)

    (test-e2e-teardown)

    (let ((passed (length (cl-remove-if-not 'identity results)))
          (total (length results)))
      (message "")
      (message "=== E2E Test Results ===")
      (message "Passed: %d/%d" passed total)
      (if (= passed total)
          (progn
            (message "✓ All E2E tests passed!")
            t)
        (progn
          (message "✗ Some E2E tests failed")
          nil)))))

;; Run tests if executed directly
(when (and (not noninteractive) (equal (buffer-name) "*scratch*"))
  (test-e2e-all))

(provide 'test-workflow)
;;; test-workflow.el ends here
