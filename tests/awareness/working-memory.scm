;;; awareness/working-memory.scm --- Working Memory Capacity Test
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Test from 14-Geometric-Theory.md Section 4.2:
;;; Validates that working memory capacity = number of independent Hopf fibers ≈ 7±2.

;;; Code:

;; Load dependencies
(load "../../scheme/r5rs-canvas-engine.scm")
(load "../../scheme/consciousness/hopf-consciousness.scm")

;; Measure Working Memory Capacity
(define (measure-wm-capacity)
  "Measure working memory capacity.
Returns capacity (number of independent items that can be held)."
  (let* ((test-state '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
         (max-fibers 10)
         (capacity 0))
    ;; Test increasing number of fibers until capacity is exceeded
    (do ((k 1 (+ k 1)))
        ((> k max-fibers) capacity)
      (let ((fiber-list (make-fiber-list k)))
        (let ((observations (parallel-observation test-state fiber-list)))
          ;; If we can successfully observe through k fibers, capacity is at least k
          (if (and (list? observations) (= (length observations) k))
              (set! capacity k)
              (set! capacity (- k 1))))))))

;; Make Fiber List Helper
(define (make-fiber-list k)
  "Create list of k fiber types."
  (let ((available-fibers '(complex quaternionic octonionic))
        (result '()))
    (do ((i 0 (+ i 1)))
        ((>= i k) (reverse result))
      (set! result (cons (list-ref available-fibers (modulo i 3)) result)))))

;; Test Working Memory Capacity
(define (test-working-memory-capacity)
  "Test working memory capacity = fiber count ≈ 7±2.
Returns test result."
  (display "Test: Working Memory Capacity\n")
  (let* ((capacity (measure-wm-capacity))
         (within-range (and (>= capacity 5) (<= capacity 9))))
    (display "  Measured capacity: ")
    (display capacity)
    (newline)
    (display "  Expected range: 5-9 (7±2)\n")
    (if within-range
        (display "  ✓ Working memory capacity within expected range\n")
        (display "  ⚠ Working memory capacity outside expected range\n"))
    (list 'working-memory-test
          `((capacity . ,capacity)
            (expected-min . 5)
            (expected-max . 9)
            (within-range . ,within-range)))))

;; Execute test
(display "==========================================\n")
(display "Working Memory Capacity Test\n")
(display "==========================================\n")
(test-working-memory-capacity)
(display "\nWorking memory test completed.\n")

