;;; test-core.el --- Core module test suite

;; Test suite for meta-log core functionality

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log)

(defun test-core-initialization ()
  "Test core system initialization."
  (message "Testing core initialization...")
  (let ((errors '()))
    (condition-case err
        (progn
          (meta-log-initialize)
          (unless meta-log--initialized-p
            (push "System should be initialized" errors)))
      (error (push (format "Initialization error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Core initialization tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Core initialization tests passed")
        t))))

(defun test-core-prolog ()
  "Test Prolog engine."
  (message "Testing Prolog engine...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test basic Prolog fact assertion
          (meta-log-prolog-assert "parent(tom, bob)")
          (meta-log-prolog-assert "parent(tom, liz)")
          (meta-log-prolog-assert "parent(bob, ann)")

          ;; Test Prolog query
          (let ((result (meta-log-prolog-query "parent(tom, X)")))
            (unless result
              (push "Prolog query should return results" errors))))
      (error (push (format "Prolog error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Prolog tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Prolog tests passed")
        t))))

(defun test-core-datalog ()
  "Test Datalog engine."
  (message "Testing Datalog engine...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test Datalog fact insertion
          (meta-log-datalog-insert-fact "edge" '("a" "b"))
          (meta-log-datalog-insert-fact "edge" '("b" "c"))

          ;; Test Datalog query
          (let ((result (meta-log-datalog-query "edge(X, Y)")))
            (unless result
              (push "Datalog query should return results" errors))))
      (error (push (format "Datalog error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Datalog tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Datalog tests passed")
        t))))

(defun test-core-m-expression ()
  "Test M-expression parsing and evaluation."
  (message "Testing M-expression engine...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test M-expression parsing
          (let ((expr (meta-log-m-expr-parse "[parent tom bob]")))
            (unless expr
              (push "M-expression should parse" errors)))

          ;; Test M-expression evaluation
          (let ((result (meta-log-m-expr-eval "[+ 1 2]")))
            (unless (equal result 3)
              (push "M-expression evaluation should work" errors))))
      (error (push (format "M-expression error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ M-expression tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ M-expression tests passed")
        t))))

(defun test-core-knowledge-graph ()
  "Test knowledge graph functionality."
  (message "Testing knowledge graph...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test node creation
          (meta-log-kg-add-node "test-node" "concept")

          ;; Test edge creation
          (meta-log-kg-add-edge "test-node" "related-to" "other-node")

          ;; Test node retrieval
          (let ((node (meta-log-kg-get-node "test-node")))
            (unless node
              (push "Should retrieve created node" errors))))
      (error (push (format "Knowledge graph error: %s" err) errors)))

    (if errors
        (progn
          (message "✗ Knowledge graph tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Knowledge graph tests passed")
        t))))

(defun test-core-all ()
  "Run all core module tests."
  (message "=== Running Core Test Suite ===")
  (let ((results '()))
    (push (test-core-initialization) results)
    (push (test-core-prolog) results)
    (push (test-core-datalog) results)
    (push (test-core-m-expression) results)
    (push (test-core-knowledge-graph) results)

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
  (test-core-all))

(provide 'test-core)
;;; test-core.el ends here
