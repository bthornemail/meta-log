;;; test-qstar-scoring.scm --- Tests for Q* Scoring Functions
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for Q* scoring functions to verify implementations.

;;; Code:

;; Load dependencies
(load "../qstar/scoring.scm")
(load "../qstar/core.scm")

;; Test helper
(define (test-passed name)
  (display (string-append "Testing " name "... PASS\n")))

(define (test-failed name reason)
  (display (string-append "Testing " name "... FAIL: " reason "\n")))

;; Test state creation
(define (make-test-state)
  "Create a test Q* state for testing."
  (make-qstar-state
   '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ()))
   '((total-memory-bytes . 100) (total-entropy . 0.5) (consistency-score . 0.8))))

(define (make-test-action)
  "Create a test Q* action for testing."
  (make-qstar-action 'transform 'binary-xor '((mask . #u8(255)))))

;; Test Euclidean scoring
(define (test-qstar-score-euclidean)
  "Test qstar-score-euclidean function."
  (let ((state (make-test-state))
        (action (make-test-action))
        (next-state (make-test-state)))
    (let ((result (qstar-score-euclidean state action next-state)))
      (if (number? result)
          (test-passed "qstar-score-euclidean")
          (test-failed "qstar-score-euclidean" "Result is not a number")))))

;; Test Weyl scoring
(define (test-qstar-score-weyl)
  "Test qstar-score-weyl function."
  (let ((state (make-test-state))
        (action (make-test-action))
        (next-state (make-test-state)))
    (let ((result (qstar-score-weyl state action next-state)))
      (if (number? result)
          (test-passed "qstar-score-weyl")
          (test-failed "qstar-score-weyl" "Result is not a number")))))

;; Test p-adic scoring
(define (test-qstar-score-padic)
  "Test qstar-score-padic function."
  (let ((state (make-test-state))
        (action (make-test-action))
        (next-state (make-test-state)))
    (let ((result (qstar-score-padic state action next-state)))
      (if (number? result)
          (test-passed "qstar-score-padic")
          (test-failed "qstar-score-padic" "Result is not a number")))))

;; Test rule compatibility scoring
(define (test-qstar-score-rule-compat)
  "Test qstar-score-rule-compat function."
  (let ((state (make-test-state))
        (action (make-test-action))
        (next-state (make-test-state)))
    (let ((result (qstar-score-rule-compat state action next-state)))
      (if (number? result)
          (test-passed "qstar-score-rule-compat")
          (test-failed "qstar-score-rule-compat" "Result is not a number")))))

;; Test resource scoring
(define (test-qstar-score-resource)
  "Test qstar-score-resource function."
  (let ((state (make-test-state))
        (action (make-test-action))
        (next-state (make-test-state)))
    (let ((result (qstar-score-resource state action next-state)))
      (if (number? result)
          (test-passed "qstar-score-resource")
          (test-failed "qstar-score-resource" "Result is not a number")))))

;; Test consensus scoring
(define (test-qstar-score-consensus)
  "Test qstar-score-consensus function."
  (let ((state (make-test-state))
        (action (make-test-action))
        (next-state (make-test-state)))
    (let ((result (qstar-score-consensus state action next-state)))
      (if (number? result)
          (test-passed "qstar-score-consensus")
          (test-failed "qstar-score-consensus" "Result is not a number")))))

;; Test complexity scoring
(define (test-qstar-score-complexity)
  "Test qstar-score-complexity function."
  (let ((state (make-test-state))
        (action (make-test-action))
        (next-state (make-test-state)))
    (let ((result (qstar-score-complexity state action next-state)))
      (if (number? result)
          (test-passed "qstar-score-complexity")
          (test-failed "qstar-score-complexity" "Result is not a number")))))

;; Test composite scoring
(define (test-qstar-compute-composite-score)
  "Test qstar-compute-composite-score function."
  (let ((state (make-test-state))
        (action (make-test-action))
        (next-state (make-test-state)))
    (let ((result (qstar-compute-composite-score state action next-state)))
      (if (and (list? result) (> (length result) 0))
          (test-passed "qstar-compute-composite-score")
          (test-failed "qstar-compute-composite-score" "Result is not a valid list")))))

;; Run all tests
(define (test-qstar-scoring-all)
  "Run all Q* scoring tests."
  (display "=== Q* Scoring Function Tests ===\n")
  (test-qstar-score-euclidean)
  (test-qstar-score-weyl)
  (test-qstar-score-padic)
  (test-qstar-score-rule-compat)
  (test-qstar-score-resource)
  (test-qstar-score-consensus)
  (test-qstar-score-complexity)
  (test-qstar-compute-composite-score)
  (display "=== Tests Complete ===\n"))

;; Run tests if executed directly
(if (not (defined? 'main))
    (test-qstar-scoring-all))

