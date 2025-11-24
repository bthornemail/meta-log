;;; test-prolog-interface.scm --- Tests for Prolog/Datalog Interface
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for Prolog/Datalog interface bridge functions.

;;; Code:

;; Test helper
(define (test-passed name)
  (display (string-append "Testing " name "... PASS\n")))

(define (test-failed name reason)
  (display (string-append "Testing " name "... FAIL: " reason "\n")))

;; Test Prolog Interface

(define (test-prolog-query)
  "Test prolog-query function."
  (let ((result (prolog-query '(test-node ?X))))
    (if (list? result)
        (test-passed "prolog-query")
        (test-failed "prolog-query" "Result is not a list"))))

(define (test-prolog-add-fact)
  "Test prolog-add-fact function."
  (let ((result (prolog-add-fact '(test-node node1))))
    (if result
        (test-passed "prolog-add-fact")
        (test-failed "prolog-add-fact" "Returned #f"))))

(define (test-prolog-add-rule)
  "Test prolog-add-rule function."
  (let ((result (prolog-add-rule '(parent ?X ?Y) '((father ?X ?Y)))))
    (if result
        (test-passed "prolog-add-rule")
        (test-failed "prolog-add-rule" "Returned #f"))))

;; Test Datalog Interface

(define (test-datalog-query)
  "Test datalog-query function."
  (let ((result (datalog-query '(edge ?X ?Y))))
    (if (list? result)
        (test-passed "datalog-query")
        (test-failed "datalog-query" "Result is not a list"))))

(define (test-datalog-add-fact)
  "Test datalog-add-fact function."
  (let ((result (datalog-add-fact '(edge "a" "b"))))
    (if result
        (test-passed "datalog-add-fact")
        (test-failed "datalog-add-fact" "Returned #f"))))

(define (test-datalog-add-rule)
  "Test datalog-add-rule function."
  (let ((result (datalog-add-rule '(ancestor ?X ?Y) '((parent ?X ?Y)))))
    (if result
        (test-passed "datalog-add-rule")
        (test-failed "datalog-add-rule" "Returned #f"))))

;; Run all tests

(define (test-prolog-interface-all)
  "Run all Prolog/Datalog interface tests."
  (display "=== Prolog/Datalog Interface Tests ===\n")
  (test-prolog-query)
  (test-prolog-add-fact)
  (test-prolog-add-rule)
  (test-datalog-query)
  (test-datalog-add-fact)
  (test-datalog-add-rule)
  (display "=== Tests Complete ===\n"))

;; Run tests if executed directly
(if (not (defined? 'main))
    (test-prolog-interface-all))

