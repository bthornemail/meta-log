;;; consciousness/state.test.scm --- Consciousness State Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for consciousness state management

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../consciousness/state.scm")

;; Test 1: Conscious State Creation
(display "Test 1: Conscious State Creation\n")
(let* ((action 10.0)
       (observation 0.5)
       (phase 0.8)
       (state (make-conscious-state action observation phase)))
  (if (and (list? state)
           (>= (length state) 5)
           (eq? (list-ref state 0) 'conscious-state)
           (= (list-ref state 1) action)
           (= (list-ref state 2) observation)
           (= (list-ref state 3) phase))
      (display "  ✓ Conscious state creation works\n")
      (begin (display "  ✗ Conscious state creation failed\n") (exit 1))))

;; Test 2: Forward Propagation (Exponential Action)
(display "Test 2: Forward Propagation\n")
(let* ((initial-action 1.0)
       (lambda 0.1)
       (epsilon 0.0)
       (next-action (conscious-action-forward initial-action lambda epsilon)))
  (if (and (number? next-action)
           (> next-action initial-action))
      (display "  ✓ Forward propagation works\n")
      (begin (display "  ✗ Forward propagation failed\n") (exit 1))))

;; Test 3: Backward Propagation (Linear Observation)
(display "Test 3: Backward Propagation\n")
(let* ((initial-observation 1.0)
       (alpha 0.9)
       (filter-fn (lambda (x) (* x 0.5)))
       (next-observation (conscious-observation-backward initial-observation alpha filter-fn)))
  (if (and (number? next-observation)
           (< next-observation initial-observation))
      (display "  ✓ Backward propagation works\n")
      (begin (display "  ✗ Backward propagation failed\n") (exit 1))))

;; Test 4: Qualia Computation
(display "Test 4: Qualia Computation\n")
(let* ((action 5.0)
       (observation 0.7)
       (phase 0.6)
       (qualia (compute-qualia action observation phase)))
  (if (and (number? qualia)
           (>= qualia 0.0))
      (display "  ✓ Qualia computation works\n")
      (begin (display "  ✗ Qualia computation failed\n") (exit 1))))

;; Test 5: Consciousness Differential
(display "Test 5: Consciousness Differential\n")
(let* ((action 2.0)
       (observation 0.8)
       (differential (conscious-differential action observation)))
  (if (number? differential)
      (display "  ✓ Consciousness differential works\n")
      (begin (display "  ✗ Consciousness differential failed\n") (exit 1))))

(display "\nAll consciousness state tests passed!\n")

