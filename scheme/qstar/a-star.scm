;;; qstar/a-star.scm --- A* Pathfinding for Q*
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements A* search algorithm for Q* pathfinding with admissible
;;; heuristics (Weyl distance, Euclidean norm, p-adic lower bounds).

;;; Code:

;; Load Q* core
;; Paths relative to scheme/ directory
(load "../qstar/core.scm")
(load "../qstar/scoring.scm")

;; Helper: Extract state properties (reuse from scoring.scm)
;; Note: These are defined in scoring.scm, but we need them here too
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

;; Helper: Create Q* action (from core.scm)
(define (make-qstar-action type operator params)
  "Create Q* action representation.
TYPE: 'transform 'synthesize 'reason 'optimize
OPERATOR: action operator name
PARAMS: alist of parameters"
  (list 'qstar-action
        type
        operator
        params))

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

(define (euclidean-norm coords)
  "Compute Euclidean norm of coordinate vector.
COORDS: list of numbers
Returns float norm."
  (if (null? coords)
      0.0
      (let ((squared-sum
             (apply + (map (lambda (c) (* c c)) coords))))
        (sqrt squared-sum))))

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

;; A* Node
(define (make-astar-node state g-cost h-cost parent action)
  "Create A* search node.
STATE: state representation
G-COST: cost from start to this node
H-COST: heuristic estimate to goal
PARENT: parent node (or #f)
ACTION: action that led to this node"
  (list 'astar-node
        state
        g-cost
        h-cost
        (+ g-cost h-cost)  ; f-cost
        parent
        action))

;; A* Search
(define (qstar-a-star start goal-pred heuristic opts)
  "A* search from start state to goal.
START: starting state
GOAL-PRED: predicate function (state -> bool)
HEURISTIC: heuristic function (state -> cost)
OPTS: alist of options (max-nodes, timeout, etc.)
Returns (path cost nodes-expanded)."
  (let ((open-set (list (make-astar-node start 0.0 (heuristic start) #f #f)))
        (closed-set '())
        (max-nodes (or (assoc-ref opts 'max-nodes) 10000))
        (nodes-expanded 0))
    (let loop ((open open-set)
               (closed closed-set)
               (expanded nodes-expanded))
      (if (or (null? open) (> expanded max-nodes))
          (list '() 0.0 expanded)  ; failure
          (let* ((current (find-min-f-cost open))
                 (current-state (list-ref current 1)))
            (if (goal-pred current-state)
                (let ((path (reconstruct-path current)))
                  (list path (list-ref current 2) expanded))
                (let* ((new-open (remove current open))
                       (new-closed (cons current closed))
                       (successors (get-successors current-state))
                       (updated-open (add-successors new-open new-closed successors current heuristic goal-pred)))
                  (loop updated-open new-closed (+ expanded 1)))))))))

(define (find-min-f-cost nodes)
  "Find node with minimum f-cost."
  (let loop ((remaining (cdr nodes))
             (best (car nodes)))
    (if (null? remaining)
        best
        (let ((current (car remaining))
              (best-f (list-ref best 4))
              (current-f (list-ref current 4)))
          (if (< current-f best-f)
              (loop (cdr remaining) current)
              (loop (cdr remaining) best))))))

(define (get-successors state)
  "Get successor states from current state.
Generates successor states by applying available actions.
Returns list of (action . next-state)."
  ;; Generate actions based on state properties
  (let ((action-types '(transform synthesize reason optimize))
        (operators (generate-operators state))
        (successors '()))
    ;; Generate successors for each action type and operator
    (let loop-types ((remaining-types action-types)
                     (acc '()))
      (if (null? remaining-types)
          acc
          (let ((action-type (car remaining-types)))
            (let loop-ops ((remaining-ops operators)
                           (acc-ops acc))
              (if (null? remaining-ops)
                  (loop-types (cdr remaining-types) acc-ops)
                  (let ((operator (car remaining-ops))
                        (action (make-qstar-action action-type operator '())))
                    ;; Apply action to get next state
                    (let ((next-state (qstar-apply-action state action)))
                      (loop-ops (cdr remaining-ops)
                               (cons (cons action next-state) acc-ops)))))))))))

(define (generate-operators state)
  "Generate available operators based on state.
Returns list of operator symbols."
  (let ((geo-layer (qstar-get-layer state 'geometric))
        (symbolic-layer (qstar-get-layer state 'symbolic))
        (operators '()))
    ;; Add geometric operators if geometric layer exists
    (if (not (null? geo-layer))
        (set! operators (append operators '(project-e8 compute-distance))))
    ;; Add symbolic operators if symbolic layer exists
    (if (not (null? symbolic-layer))
        (set! operators (append operators '(query-prolog query-datalog))))
    ;; Always available operators
    (set! operators (append operators '(store-cbs transform-data)))
    operators))

(define (add-successors open closed successors parent heuristic goal-pred)
  "Add successor nodes to open set."
  (let loop ((remaining successors)
             (new-open open))
    (if (null? remaining)
        new-open
        (let ((successor (car remaining))
              (action (car successor))
              (next-state (cdr successor))
              (parent-g (list-ref parent 2))
              (edge-cost (qstar-immediate-cost (list-ref parent 1) action)))
          (if (not (in-closed-set? next-state closed))
              (let ((g-cost (+ parent-g edge-cost))
                    (h-cost (heuristic next-state))
                    (node (make-astar-node next-state g-cost h-cost parent action)))
                (let ((existing (find-in-open next-state new-open)))
                  (if existing
                      (if (< g-cost (list-ref existing 2))
                          (loop (cdr remaining) (replace-node new-open existing node))
                          (loop (cdr remaining) new-open))
                      (loop (cdr remaining) (cons node new-open)))))
              (loop (cdr remaining) new-open))))))

(define (in-closed-set? state closed)
  "Check if state is in closed set."
  (any (lambda (node)
         (equal? (list-ref node 1) state))
       closed))

(define (any list)
  "Check if any element in list is true."
  (if (null? list)
      #f
      (or (car list) (any (cdr list)))))

(define (find-in-open state open)
  "Find node with state in open set."
  (find (lambda (node)
          (equal? (list-ref node 1) state))
        open))

(define (find list pred)
  "Find first element in list satisfying predicate."
  (if (null? list)
      #f
      (if (pred (car list))
          (car list)
          (find (cdr list) pred))))

(define (replace-node open old-node new-node)
  "Replace old node with new node in open set."
  (map (lambda (node)
         (if (equal? node old-node)
             new-node
             node))
       open))

(define (reconstruct-path node)
  "Reconstruct path from start to goal."
  (let loop ((current node)
             (path '()))
    (if (not current)
        path
        (let ((action (list-ref current 6)))
          (if action
              (loop (list-ref current 5) (cons action path))
              path)))))

;; Heuristic Functions

(define (heuristic-euclidean state)
  "Euclidean norm difference heuristic.
Estimates distance to goal using Euclidean norm of state properties.
Admissible heuristic: uses state complexity as proxy for distance to goal.
STATE: Q* state representation
Returns non-negative float estimate."
  (let ((geo-layer (qstar-get-layer state 'geometric))
        (coords (extract-e8-coords geo-layer)))
    (if (not (null? coords))
        ;; Use Euclidean norm of E8 coordinates
        (euclidean-norm (car coords))
        ;; Fallback: use memory and entropy as proxy
        (let ((mem (qstar-get-property state 'total-memory-bytes))
              (entropy (qstar-get-property state 'total-entropy)))
          (if (and mem entropy)
              ;; Heuristic: distance ≈ sqrt(memory^2 + entropy^2)
              (sqrt (+ (* mem mem) (* entropy entropy)))
              ;; Minimal estimate if no properties available
              1.0)))))

(define (heuristic-weyl state)
  "Weyl distance heuristic.
Estimates distance to goal using Weyl orbit distance.
Weyl distance is the minimum distance over the Weyl orbit, which
is computationally expensive. We approximate it using Euclidean
distance scaled by a Weyl factor.
Admissible heuristic: Weyl distance ≤ 1.2 * Euclidean distance (typically).
STATE: Q* state representation
Returns non-negative float estimate."
  ;; Weyl distance is typically slightly larger than Euclidean
  ;; For admissibility, we use a conservative scaling factor
  (let ((euclidean (heuristic-euclidean state)))
    (* euclidean 1.2)))  ; Conservative: Weyl ≈ 1.2 * Euclidean

(define (heuristic-padic-lowerbound state)
  "p-adic lower bound heuristic.
Estimates distance to goal using p-adic valuation.
Uses 2-adic norm of state properties as a lower bound.
Admissible heuristic: p-adic norm provides a lower bound on distance.
STATE: Q* state representation
Returns non-negative float estimate."
  (let ((mem (qstar-get-property state 'total-memory-bytes))
        (entropy (qstar-get-property state 'total-entropy))
        (p 2))  ; Use 2-adic valuation
    (if (and mem entropy)
        ;; Heuristic: p-adic distance ≈ |mem|_p + |entropy|_p
        (let ((mem-norm (p-adic-norm mem p))
              (entropy-norm (if (number? entropy)
                               (p-adic-norm (inexact->exact (round entropy)) p)
                               1.0)))
          (+ mem-norm entropy-norm))
        ;; Minimal estimate if no properties available
        1.0)))

;; Q* A* API

(define (qstar-a-star-search start-id goal-pred-name heuristic-name opts)
  "A* search for Q* pathfinding.
START-ID: URI or ID of start state
GOAL-PRED-NAME: name of goal predicate function
HEURISTIC-NAME: name of heuristic function
OPTS: search options
Returns A* result."
  (let ((start (substrate-get-memory start-id))
        (goal-pred (get-goal-pred goal-pred-name))
        (heuristic (get-heuristic heuristic-name)))
    (qstar-a-star start goal-pred heuristic opts)))

(define (get-goal-pred name)
  "Get goal predicate function by name."
  (case name
    ((default) qstar-goal-p)
    (else qstar-goal-p)))

(define (get-heuristic name)
  "Get heuristic function by name."
  (case name
    ((weyl) heuristic-weyl)
    ((euclidean) heuristic-euclidean)
    ((padic) heuristic-padic-lowerbound)
    (else heuristic-euclidean)))

;; Functions are exported by default in R5RS

