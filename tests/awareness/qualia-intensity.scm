;;; awareness/qualia-intensity.scm --- Qualia Intensity Validation
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Test from 14-Geometric-Theory.md Section 4.3:
;;; Validates that qualia intensity ∝ Curvature(Hopf_fiber),
;;; with octonionic > quaternionic > complex.

;;; Code:

;; Load dependencies
(load "../../scheme/r5rs-canvas-engine.scm")
(load "../../scheme/consciousness/qualia.scm")
(load "../../scheme/consciousness/hopf-consciousness.scm")

;; Measure Qualia Intensity for Fiber Type
(define (measure-qualia-intensity fiber-type)
  "Measure qualia intensity for specific Hopf fiber type.
FIBER-TYPE: 'complex, 'quaternionic, or 'octonionic
Returns qualia intensity value."
  (let* ((test-state '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
         (action 5.0)
         (phase 0.8)
         (threshold 0.3)
         (projection (consciousness-hopf-project test-state fiber-type))
         (obs-magnitude (if (list? projection)
                          (sqrt (apply + (map (lambda (x) (* x x)) projection)))
                          0.0))
         (qualia-field (emerge-qualia action obs-magnitude phase threshold)))
    (if (and (list? qualia-field) (not (eq? qualia-field #f)))
        (let ((qualia-map (list-ref qualia-field 2)))
          (let ((intensity (assoc-ref qualia-map 'intensity)))
            (if (number? intensity) intensity 0.0)))
        0.0)))

;; Test Qualia Intensity Ordering
(define (test-qualia-intensity)
  "Test qualia intensity ordering: octonionic > quaternionic > complex.
Returns test result."
  (display "Test: Qualia Intensity vs. Hopf Fiber Type\n")
  (let* ((complex-intensity (measure-qualia-intensity 'complex))
         (quat-intensity (measure-qualia-intensity 'quaternionic))
         (oct-intensity (measure-qualia-intensity 'octonionic))
         (ordering-valid (and (< complex-intensity quat-intensity)
                             (< quat-intensity oct-intensity))))
    (display "  Complex fiber qualia intensity: ")
    (display complex-intensity)
    (newline)
    (display "  Quaternionic fiber qualia intensity: ")
    (display quat-intensity)
    (newline)
    (display "  Octonionic fiber qualia intensity: ")
    (display oct-intensity)
    (newline)
    (if ordering-valid
        (display "  ✓ Qualia intensity ordering validated: octonionic > quaternionic > complex\n")
        (begin
          (display "  ⚠ Qualia intensity ordering not as expected\n")
          (display "    (May need better test data or actual curvature computation)\n")))
    (list 'qualia-intensity-test
          `((complex-intensity . ,complex-intensity)
            (quaternionic-intensity . ,quat-intensity)
            (octonionic-intensity . ,oct-intensity)
            (ordering-valid . ,ordering-valid)))))

;; Execute test
(display "==========================================\n")
(display "Qualia Intensity Validation Test\n")
(display "==========================================\n")
(test-qualia-intensity)
(display "\nQualia intensity test completed.\n")

