;;; test-workflow.el --- End-to-end workflow test suite

;; E2E test suite for meta-log complete workflows

;; Add load path
(add-to-list 'load-path (expand-file-name "../../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log)
(require 'meta-log-ingest)
(require 'meta-log-knowledge-graph)
(require 'meta-log-llm)
(require 'meta-log-chat)

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
          (meta-log-ingest-file test-org-file)

          ;; Verify knowledge graph nodes were created
          (let ((ml-node (meta-log-kg-get-node "Machine Learning"))
                (lisp-node (meta-log-kg-get-node "Lisp")))
            (unless ml-node
              (push "Should create ML node in knowledge graph" errors))
            (unless lisp-node
              (push "Should create Lisp node in knowledge graph" errors)))

          ;; Test search functionality
          (let ((results (meta-log-kg-search "machine")))
            (unless results
              (push "Should find search results" errors))))
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
          (meta-log-prolog-assert "programming_language(lisp)")
          (meta-log-prolog-assert "programming_language(python)")
          (meta-log-prolog-assert "paradigm(lisp, functional)")
          (meta-log-prolog-assert "paradigm(python, imperative)")

          ;; Test M-expression query
          (let ((result (meta-log-m-expr-eval "[query [programming_language X]]")))
            (unless result
              (push "M-expression query should return results" errors)))

          ;; Test natural language query
          (let ((response (meta-log-ask "What programming languages are in the system?")))
            (unless response
              (push "Natural language query should return response" errors))))
      (error (push (format "Query workflow error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Query workflow failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Query workflow passed")
        t))))

(defun test-e2e-chat-workflow ()
  "Test complete chat interaction workflow."
  (message "Testing chat workflow...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Initialize system
          (unless meta-log--initialized-p
            (meta-log-initialize))

          ;; Test chat message sending
          (let ((response (meta-log-chat-send "Hello, can you help me?")))
            (unless response
              (push "Chat should return response" errors)))

          ;; Test chat history
          (let ((history (meta-log-chat-get-history)))
            (unless history
              (push "Chat should maintain history" errors))
            (unless (> (length history) 0)
              (push "Chat history should contain messages" errors))))
      (error (push (format "Chat workflow error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Chat workflow failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Chat workflow passed")
        t))))

(defun test-e2e-learning-workflow ()
  "Test complete learning and adaptation workflow."
  (message "Testing learning workflow...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Initialize system
          (unless meta-log--initialized-p
            (meta-log-initialize))

          ;; Teach the system something
          (meta-log-llm-record-interaction
           "What is meta-programming?"
           "Meta-programming is writing code that generates or manipulates code.")

          ;; Ask related question
          (let ((response (meta-log-ask "Tell me about meta-programming")))
            (unless response
              (push "Should leverage learned knowledge" errors))
            (unless (string-match-p "code" response)
              (push "Response should reference learned information" errors))))
      (error (push (format "Learning workflow error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Learning workflow failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Learning workflow passed")
        t))))

(defun test-e2e-all ()
  "Run all e2e workflow tests."
  (message "=== Running E2E Test Suite ===")
  (test-e2e-setup)
  (let ((results '()))
    (push (test-e2e-knowledge-ingestion) results)
    (push (test-e2e-query-workflow) results)
    (push (test-e2e-chat-workflow) results)
    (push (test-e2e-learning-workflow) results)

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
