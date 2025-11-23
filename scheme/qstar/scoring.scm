;;; qstar/scoring.scm --- Q* Cost Function Registry
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements composable scoring functions for Q* cost evaluation.

;;; Code:

;; Scoring Function Registry
(define *scoring-registry* '())

(define (qstar-register-scoring-fn name fn priority)
  "Register a scoring function.
NAME: symbol identifying the function
FN: function taking (state action next-state) -> cost
PRIORITY: integer priority (higher = evaluated first)"
  (set! *scoring-registry*
        (cons (list name fn priority)
              *scoring-registry*))
  ;; Sort by priority
  (set! *scoring-registry*
        (sort-scoring-fns *scoring-registry*)))

(define (sort-scoring-fns registry)
  "Sort scoring functions by priority."
  (let loop ((remaining registry)
             (sorted '()))
    (if (null? remaining)
        (reverse sorted)
        (let ((best (find-highest-priority remaining)))
          (loop (remove best remaining)
                (cons best sorted))))))

(define (find-highest-priority registry)
  "Find function with highest priority."
  (let loop ((remaining (cdr registry))
             (best (car registry)))
    (if (null? remaining)
        best
        (let* ((current (car remaining))
               (best-pri (list-ref best 2))
               (current-pri (list-ref current 2)))
          (if (> current-pri best-pri)
              (loop (cdr remaining) current)
              (loop (cdr remaining) best))))))

(define (remove item list)
  "Remove first occurrence of item from list."
  (if (null? list)
      '()
      (if (equal? (car list) item)
          (cdr list)
          (cons (car list) (remove item (cdr list))))))

;; Compute composite score
(define (qstar-compute-composite-score state action next-state)
  "Compute composite score using all registered functions.
Returns alist of (name . cost)."
  (map (lambda (entry)
         (let ((name (list-ref entry 0))
               (fn (list-ref entry 1)))
           (cons name (fn state action next-state))))
       *scoring-registry*))

;; Built-in Scoring Functions

(define (qstar-score-euclidean state action next-state)
  "Euclidean distance cost (for E8 coordinates)."
  ;; Placeholder
  0.0)

(define (qstar-score-weyl state action next-state)
  "Weyl distance cost."
  ;; Placeholder
  0.0)

(define (qstar-score-padic state action next-state)
  "p-adic valuation cost."
  ;; Placeholder
  0.0)

(define (qstar-score-rule-compat state action next-state)
  "Rule compatibility cost."
  ;; Placeholder
  0.0)

(define (qstar-score-resource state action next-state)
  "Resource usage cost."
  ;; Placeholder
  0.0)

(define (qstar-score-consensus state action next-state)
  "Consensus penalty cost."
  ;; Placeholder
  0.0)

(define (qstar-score-complexity state action next-state)
  "Complexity penalty cost."
  ;; Placeholder
  0.0)

;; Register built-in functions
(qstar-register-scoring-fn 'euclidean qstar-score-euclidean 10)
(qstar-register-scoring-fn 'weyl qstar-score-weyl 10)
(qstar-register-scoring-fn 'padic qstar-score-padic 8)
(qstar-register-scoring-fn 'rule-compat qstar-score-rule-compat 9)
(qstar-register-scoring-fn 'resource qstar-score-resource 7)
(qstar-register-scoring-fn 'consensus qstar-score-consensus 6)
(qstar-register-scoring-fn 'complexity qstar-score-complexity 5)

;; Functions are exported by default in R5RS

