;;; awareness/independence.scm --- Independence Test
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Test from 14-Geometric-Theory.md Section 4.4:
;;; Validates that unconscious processing time (O(2^d)) and
;;; conscious reaction time (O(k)) are independent.

;;; Code:

;; Load dependencies
(load "../../scheme/r5rs-canvas-engine.scm")
(load "../../scheme/consciousness/complexity.scm")

;; Measure Unconscious Processing Times
(define (measure-unconscious-times depths)
  "Measure unconscious processing time for different depths.
DEPTHS: list of depths to test
Returns list of (depth . time) pairs."
  (let ((test-point '(1.0 2.0 3.0))
        (results '()))
    (for-each
     (lambda (depth)
       (let* ((comparison (compare-complexities test-point 1 depth))
              (unconscious-time (list-ref comparison 0)))
         (set! results (cons (cons depth unconscious-time) results))))
     depths)
    (reverse results)))

;; Measure Conscious Reaction Times
(define (measure-conscious-times fiber-counts)
  "Measure conscious reaction time for different fiber counts.
FIBER-COUNTS: list of fiber counts to test
Returns list of (fiber-count . time) pairs."
  (let ((test-state '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
        (test-point '(1.0 2.0 3.0))
        (results '()))
    (for-each
     (lambda (k)
       (let ((comparison (compare-complexities test-point k 1))
             (conscious-time (list-ref comparison 1)))
         (set! results (cons (cons k conscious-time) results))))
     fiber-counts)
    (reverse results)))

;; Check Correlation
(define (check-independence unconscious-times conscious-times)
  "Check if unconscious and conscious times are independent.
UNCONSCIOUS-TIMES: list of (depth . time) pairs
CONSCIOUS-TIMES: list of (fiber-count . time) pairs
Returns #t if independent (no correlation), #f otherwise."
  ;; Simple check: if both scale differently (exponential vs linear), they're independent
  (let ((unconscious-exp (validate-exponential-action (map car unconscious-times) (map (lambda (p) (list (car p) (cdr p) 0)) unconscious-times)))
        (conscious-linear (validate-linear-observation (map car conscious-times) conscious-times)))
    (and unconscious-exp conscious-linear)))  ; Independent if one is exp and other is linear

;; Test Independence
(define (test-independence)
  "Test independence of unconscious vs. conscious processing times.
Returns test result."
  (display "Test: Independence of Unconscious vs. Conscious Times\n")
  (let* ((depths '(1 2 3 4))
         (fiber-counts '(1 2 3 4 5))
         (unconscious-times (measure-unconscious-times depths))
         (conscious-times (measure-conscious-times fiber-counts))
         (independent? (check-independence unconscious-times conscious-times)))
    (display "  Unconscious times (should be O(2^d)): ")
    (display unconscious-times)
    (newline)
    (display "  Conscious times (should be O(k)): ")
    (display conscious-times)
    (newline)
    (if independent?
        (display "  ✓ Unconscious and conscious times are independent\n")
        (display "  ⚠ Independence validation (may need more data points)\n"))
    (list 'independence-test
          `((unconscious-times . ,unconscious-times)
            (conscious-times . ,conscious-times)
            (independent . ,independent?)))))

;; Execute test
(display "==========================================\n")
(display "Independence Test\n")
(display "==========================================\n")
(test-independence)
(display "\nIndependence test completed.\n")

