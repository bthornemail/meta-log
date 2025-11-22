;;; qstar/scoring.test.scm --- Q* Scoring Function Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for Q* scoring function registry

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../qstar/scoring.scm")

;; Test 1: Scoring Function Registration
(display "Test 1: Scoring Function Registration\n")
(let ((test-fn (lambda (s a s2) 1.0))
      (initial-count (length *scoring-registry*)))
  (qstar-register-scoring-fn 'test-scoring test-fn 15)
  (if (> (length *scoring-registry*) initial-count)
      (display "  ✓ Function registration works\n")
      (begin (display "  ✗ Function registration failed\n") (exit 1))))

;; Test 2: Priority Sorting
(display "Test 2: Priority Sorting\n")
(let* ((test-fn (lambda (s a s2) 1.0))
       (registry-before *scoring-registry*)
       (registered (qstar-register-scoring-fn 'high-priority test-fn 20))
       (first-entry (car *scoring-registry*))
       (first-priority (list-ref first-entry 2)))
  (if (>= first-priority 20)
      (display "  ✓ Priority sorting works\n")
      (begin (display "  ✗ Priority sorting failed\n") (exit 1))))

;; Test 3: Composite Score Computation
(display "Test 3: Composite Score Computation\n")
(let* ((state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ())))
       (action '(transform binary-xor))
       (next-state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ())))
       (scores (qstar-compute-composite-score state action next-state)))
  (if (and (list? scores)
           (> (length scores) 0)
           (assoc-ref scores 'euclidean)
           (assoc-ref scores 'weyl)
           (assoc-ref scores 'padic))
      (display "  ✓ Composite score computation works\n")
      (begin (display "  ✗ Composite score computation failed\n") (exit 1))))

;; Test 4: Built-in Scoring Functions
(display "Test 4: Built-in Scoring Functions\n")
(let* ((state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ())))
       (action '(transform binary-xor))
       (next-state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ())))
       (euclidean (qstar-score-euclidean state action next-state))
       (weyl (qstar-score-weyl state action next-state))
       (padic (qstar-score-padic state action next-state))
       (complexity (qstar-score-complexity state action next-state)))
    (if (and (number? euclidean)
             (number? weyl)
             (number? padic)
             (number? complexity)
             (>= euclidean 0)
             (>= weyl 0)
             (>= padic 0)
             (>= complexity 0))
        (display "  ✓ Built-in scoring functions work\n")
        (begin (display "  ✗ Built-in scoring functions failed\n") (exit 1)))))

(display "\nAll Q* scoring tests passed!\n")

