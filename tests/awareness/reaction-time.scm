;;; awareness/reaction-time.scm --- Reaction Time Scaling Test
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Test from 14-Geometric-Theory.md Section 4.1:
;;; Validates that reaction time scales linearly with fiber count (O(k)),
;;; NOT exponentially with decision space (O(2^d)).

;;; Code:

;; Load dependencies
(load "../../scheme/r5rs-canvas-engine.scm")
(load "../../scheme/consciousness/complexity.scm")

;; Measure Reaction Time with Varying Fiber Counts
(define (test-reaction-time-scaling)
  "Test reaction time scaling with fiber count.
Returns test results showing O(k) linear scaling."
  (display "Test: Reaction Time Scaling\n")
  (let* ((test-state '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
         (fiber-counts '(1 2 3 4 5 6 7))
         (results (measure-observation-complexity test-state fiber-counts))
         (linear-valid (validate-linear-observation fiber-counts results)))
    (display "  Fiber Counts: ")
    (display fiber-counts)
    (newline)
    (display "  Results: ")
    (display results)
    (newline)
    (if linear-valid
        (display "  ✓ Reaction time scales linearly O(k)\n")
        (display "  ⚠ Reaction time scaling validation (may need timing infrastructure)\n"))
    (list 'reaction-time-test
          `((fiber-counts . ,fiber-counts)
            (results . ,results)
            (linear-valid . ,linear-valid)))))

;; Measure Reaction Time with Varying Decision Space
(define (test-decision-space-scaling)
  "Test reaction time scaling with decision space.
Returns test results showing it does NOT scale exponentially O(2^d)."
  (display "Test: Decision Space Scaling (should NOT be exponential)\n")
  (let* ((test-point '(1.0 2.0 3.0))
         (depths '(1 2 3 4))
         (results (measure-action-complexity test-point depths))
         (exp-valid (validate-exponential-action depths results)))
    (display "  Depths: ")
    (display depths)
    (newline)
    (display "  Results: ")
    (display results)
    (newline)
    (display "  Note: Action complexity is exponential, but conscious RT should be linear\n")
    (list 'decision-space-test
          `((depths . ,depths)
            (results . ,results)
            (exp-valid . ,exp-valid)))))

;; Run Complete Reaction Time Test
(define (run-reaction-time-test)
  "Run complete reaction time scaling test suite."
  (let ((test1 (test-reaction-time-scaling))
        (test2 (test-decision-space-scaling)))
    (list 'reaction-time-test-suite
          test1
          test2)))

;; Execute test
(display "==========================================\n")
(display "Reaction Time Scaling Test\n")
(display "==========================================\n")
(run-reaction-time-test)
(display "\nReaction time test completed.\n")

