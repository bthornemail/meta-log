;;; physics/gr.test.scm --- General Relativity Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for General Relativity from E8

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../physics/gr.scm")

;; Helper: make-list
(define (make-list n value)
  "Create list of n elements with value."
  (let loop ((i 0) (result '()))
    (if (>= i n)
        (reverse result)
        (loop (+ i 1) (cons value result)))))

;; Test 1: Einstein Equations Structure
(display "Test 1: Einstein Equations Structure\n")
(let* ((energy-momentum (make-list 4 (make-list 4 1.0)))
       (equations (einstein-equations energy-momentum)))
  (if (and (list? equations)
           (>= (length equations) 3)
           (eq? (list-ref equations 0) 'field-equations))
      (display "  ✓ Einstein equations structure works\n")
      (begin (display "  ✗ Einstein equations structure failed\n") (exit 1))))

;; Test 2: Ricci Tensor Computation
(display "Test 2: Ricci Tensor Computation\n")
(let* ((energy-momentum (make-list 4 (make-list 4 0.5)))
       (ricci (compute-ricci energy-momentum)))
  (if (and (list? ricci)
           (= (length ricci) 4))
      (display "  ✓ Ricci tensor computation works\n")
      (begin (display "  ✗ Ricci tensor computation failed\n") (exit 1))))

;; Test 3: E8 to Metric Mapping
(display "Test 3: E8 to Metric Mapping\n")
(let* ((energy-momentum (make-list 4 (make-list 4 1.0)))
       (metric (e8-to-metric energy-momentum)))
  (if (and (list? metric)
           (= (length metric) 4))
      (display "  ✓ E8 to metric mapping works\n")
      (begin (display "  ✗ E8 to metric mapping failed\n") (exit 1))))

(display "\nAll General Relativity tests passed!\n")

