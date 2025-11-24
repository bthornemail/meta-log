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

;; Helper functions for extracting data from state

(define (qstar-get-layer state layer-name)
  "Extract layer from state.
LAYER-NAME: 'binary 'waveform 'geometric 'symbolic
Returns layer data or empty list."
  (let ((layers (list-ref state 2)))  ; layers is at index 2
    (let ((layer-pair (assoc layer-name layers)))
      (if layer-pair
          (cdr layer-pair)
          '()))))

(define (qstar-get-property state prop-name)
  "Extract property from state global properties.
PROP-NAME: property name symbol
Returns property value or #f."
  (let ((props (list-ref state 3)))  ; global-properties is at index 3
    (let ((prop-pair (assoc prop-name props)))
      (if prop-pair
          (cdr prop-pair)
          #f))))

(define (euclidean-distance coords1 coords2)
  "Compute Euclidean distance between two coordinate lists.
COORDS1, COORDS2: lists of numbers (same length)
Returns float distance."
  (if (or (null? coords1) (null? coords2))
      0.0
      (let ((squared-sum
             (apply + (map (lambda (c1 c2)
                            (let ((diff (- c1 c2)))
                              (* diff diff)))
                          coords1 coords2))))
        (sqrt squared-sum))))

(define (extract-e8-coords geometric-layer)
  "Extract E8 coordinates from geometric layer.
GEOMETRIC-LAYER: list of geometric objects
Returns list of 8D coordinate lists, or empty list."
  (if (null? geometric-layer)
      '()
      (let ((first-obj (car geometric-layer)))
        ;; Check if object has E8 coordinates
        (if (and (list? first-obj)
                 (>= (length first-obj) 2)
                 (eq? (car first-obj) 'e8-point))
            ;; Extract coordinates (assuming format: (e8-point coords ...))
            (let ((coords (list-ref first-obj 1)))
              (if (and (list? coords) (= (length coords) 8))
                  (list coords)
                  '()))
            '()))))

(define (p-adic-valuation n p)
  "Compute p-adic valuation v_p(n).
N: integer
P: prime number
Returns highest power of p dividing n."
  (if (or (zero? n) (< n 0))
      0
      (let loop ((num (abs n))
                 (count 0))
        (if (or (zero? num) (not (zero? (modulo num p))))
            count
            (loop (quotient num p) (+ count 1))))))

(define (p-adic-norm x p)
  "Compute p-adic norm |x|_p = p^{-v_p(x)}.
X: integer
P: prime number
Returns float."
  (let ((val (p-adic-valuation x p)))
    (if (zero? val)
        1.0
        (expt p (- val)))))

;; Built-in Scoring Functions

(define (qstar-score-euclidean state action next-state)
  "Euclidean distance cost (for E8 coordinates).
Computes distance between geometric layers of state and next-state."
  (let ((geo-state (qstar-get-layer state 'geometric))
        (geo-next (qstar-get-layer next-state 'geometric))
        (coords-state (extract-e8-coords geo-state))
        (coords-next (extract-e8-coords geo-next)))
    (if (and (not (null? coords-state)) (not (null? coords-next)))
        ;; Compute distance between first E8 points
        (euclidean-distance (car coords-state) (car coords-next))
        ;; Fallback: use memory difference as proxy
        (let ((mem-state (qstar-get-property state 'total-memory-bytes))
              (mem-next (qstar-get-property next-state 'total-memory-bytes)))
          (if (and mem-state mem-next)
              (abs (- mem-state mem-next))
              0.0)))))

(define (qstar-score-weyl state action next-state)
  "Weyl distance cost.
Computes approximate Weyl distance using Euclidean distance as proxy.
Full Weyl orbit computation would be expensive, so we use a heuristic."
  ;; Weyl distance is minimum distance over Weyl orbit
  ;; For efficiency, we approximate with Euclidean distance scaled
  (let ((euclidean (qstar-score-euclidean state action next-state)))
    (* euclidean 1.2)))  ; Heuristic: Weyl distance ≈ 1.2 * Euclidean

(define (qstar-score-padic state action next-state)
  "p-adic valuation cost.
Computes p-adic norm difference for state transitions."
  (let ((mem-state (qstar-get-property state 'total-memory-bytes))
        (mem-next (qstar-get-property next-state 'total-memory-bytes)))
    (if (and mem-state mem-next)
        (let ((p 2))  ; Use 2-adic valuation
          (abs (- (p-adic-norm mem-state p)
                  (p-adic-norm mem-next p))))
        0.0)))

(define (qstar-score-rule-compat state action next-state)
  "Rule compatibility cost.
Checks if action is compatible with Prolog/Datalog rules.
Uses prolog-query to check rule violations."
  (let ((action-type (list-ref action 1))  ; action type
        (action-op (list-ref action 2))    ; operator
        (symbolic-state (qstar-get-layer state 'symbolic))
        (symbolic-next (qstar-get-layer next-state 'symbolic)))
    ;; Check if action violates any rules
    ;; Query: (allowed-action action-type action-op)
    (let ((query-result (prolog-query `(allowed-action ,action-type ,action-op))))
      (if (null? query-result)
          ;; No rule allows this action - penalty
          1.0
          ;; Action is allowed - no penalty
          0.0))))

(define (qstar-score-resource state action next-state)
  "Resource usage cost.
Computes cost based on memory and computational resources."
  (let ((mem-state (qstar-get-property state 'total-memory-bytes))
        (mem-next (qstar-get-property next-state 'total-memory-bytes))
        (entropy-state (qstar-get-property state 'total-entropy))
        (entropy-next (qstar-get-property next-state 'total-entropy)))
    (let ((mem-delta (if (and mem-state mem-next)
                         (abs (- mem-next mem-state))
                         0))
          (entropy-delta (if (and entropy-state entropy-next)
                            (abs (- entropy-next entropy-state))
                            0.0)))
      ;; Combine memory and entropy costs
      (+ (* mem-delta 0.0001)  ; Memory cost: 0.0001 per byte
         (* entropy-delta 0.1)))))  ; Entropy cost: 0.1 per unit

(define (qstar-score-consensus state action next-state)
  "Consensus penalty cost.
Penalizes actions that reduce consistency/consensus."
  (let ((consistency-state (qstar-get-property state 'consistency-score))
        (consistency-next (qstar-get-property next-state 'consistency-score)))
    (if (and consistency-state consistency-next)
        (let ((delta (- consistency-state consistency-next)))
          ;; Penalize if consistency decreases
          (if (> delta 0)
              delta  ; Penalty proportional to consistency loss
              0.0))
        0.0)))

(define (qstar-score-complexity state action next-state)
  "Complexity penalty cost.
Penalizes actions that increase system complexity."
  (let ((entropy-state (qstar-get-property state 'total-entropy))
        (entropy-next (qstar-get-property next-state 'total-entropy))
        (mem-state (qstar-get-property state 'total-memory-bytes))
        (mem-next (qstar-get-property next-state 'total-memory-bytes)))
    (let ((entropy-delta (if (and entropy-state entropy-next)
                             (- entropy-next entropy-state)
                             0.0))
          (mem-delta (if (and mem-state mem-next)
                        (- mem-next mem-state)
                        0)))
      ;; Complexity increases with entropy and memory growth
      (max 0.0 (+ (* entropy-delta 0.5)
                  (* mem-delta 0.00001))))))

;; Register built-in functions
(qstar-register-scoring-fn 'euclidean qstar-score-euclidean 10)
(qstar-register-scoring-fn 'weyl qstar-score-weyl 10)
(qstar-register-scoring-fn 'padic qstar-score-padic 8)
(qstar-register-scoring-fn 'rule-compat qstar-score-rule-compat 9)
(qstar-register-scoring-fn 'resource qstar-score-resource 7)
(qstar-register-scoring-fn 'consensus qstar-score-consensus 6)
(qstar-register-scoring-fn 'complexity qstar-score-complexity 5)

;; Functions are exported by default in R5RS

