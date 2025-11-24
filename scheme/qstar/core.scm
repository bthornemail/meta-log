;;; qstar/core.scm --- Q* Optimality Engine Core
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements Q* optimality evaluation using Bellman optimality and
;;; dynamic programming as specified in Q* proposals.

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")
(load "../substrate/prolog-interface.scm")

;; Helper: Extract property from state (needed for cost functions)
(define (qstar-get-property state prop-name)
  "Extract property from state global properties.
PROP-NAME: property name symbol
Returns property value or #f."
  (let ((props (list-ref state 3)))  ; global-properties is at index 3
    (let ((prop-pair (assoc prop-name props)))
      (if prop-pair
          (cdr prop-pair)
          #f))))

;; Q* Configuration
(define *qstar-gamma* 0.95)  ; discount factor
(define *qstar-weights* '((geo . 1.0)
                         (padic . 0.8)
                         (rule . 2.0)
                         (consensus . 1.5)
                         (complexity . 0.5)))

;; State Representation
(define (make-qstar-state layers global-properties)
  "Create Q* state representation.
LAYERS: alist with binary, waveform, geometric, symbolic lists
GLOBAL-PROPERTIES: alist with total-memory, total-entropy, consistency-score"
  (list 'qstar-state
        (uuid-generate)
        layers
        global-properties))

;; Action Representation
(define (make-qstar-action type operator params)
  "Create Q* action representation.
TYPE: 'transform 'synthesize 'reason 'optimize
OPERATOR: action operator name
PARAMS: alist of parameters"
  (list 'qstar-action
        type
        operator
        params))

;; Q-Value Computation
(define (qstar-evaluate state action)
  "Evaluate Q*(state, action).
Returns (value plan provenance)."
  (let* ((immediate-cost (qstar-immediate-cost state action))
         (next-state (qstar-apply-action state action))
         (future-value (if (qstar-goal-p next-state)
                           0.0
                           (qstar-future-value next-state))))
    (let ((q-value (- (+ immediate-cost (* *qstar-gamma* future-value)))))
      (list q-value
            (list action)
            `((immediate-cost . ,immediate-cost)
              (future-value . ,future-value)
              (gamma . ,*qstar-gamma*))))))

(define (qstar-immediate-cost state action)
  "Compute immediate cost of action in state."
  (let ((costs (qstar-compute-costs state action)))
    (apply + (map cdr costs))))

(define (qstar-compute-costs state action)
  "Compute cost breakdown for action.
Returns alist of (component . cost)."
  (let ((computational (qstar-computational-cost state action))
        (memory (qstar-memory-cost state action))
        (entropy (qstar-entropy-cost state action))
        (complexity (qstar-complexity-cost state action))
        (safety (qstar-safety-penalty state action)))
    `((computational . ,computational)
      (memory . ,memory)
      (entropy . ,entropy)
      (complexity . ,complexity)
      (safety . ,safety))))

(define (qstar-computational-cost state action)
  "Compute computational cost component.
Based on action type and operator complexity."
  (let ((action-type (list-ref action 1))
        (operator (list-ref action 2))
        (entropy (qstar-get-property state 'total-entropy)))
    ;; Base cost by action type
    (let ((base-cost (case action-type
                       ((transform) 0.2)
                       ((synthesize) 0.3)
                       ((reason) 0.15)
                       ((optimize) 0.25)
                       (else 0.1))))
      ;; Scale by entropy (higher entropy = more computation)
      (let ((entropy-factor (if entropy
                               (* entropy 0.01)
                               0.0)))
        (+ base-cost entropy-factor)))))

(define (qstar-memory-cost state action)
  "Compute memory cost component.
Based on memory usage and action memory requirements."
  (let ((mem-state (qstar-get-property state 'total-memory-bytes))
        (action-type (list-ref action 1)))
    ;; Base memory cost by action type
    (let ((base-cost (case action-type
                       ((transform) 0.05)
                       ((synthesize) 0.1)
                       ((reason) 0.03)
                       ((optimize) 0.08)
                       (else 0.05))))
      ;; Scale by current memory (normalized)
      (let ((mem-factor (if mem-state
                           (* mem-state 0.000001)  ; 1MB = 0.001 cost
                           0.0)))
        (+ base-cost mem-factor)))))

(define (qstar-entropy-cost state action)
  "Compute entropy cost component.
Based on entropy increase from action."
  (let ((entropy-state (qstar-get-property state 'total-entropy))
        (action-type (list-ref action 1)))
    ;; Base entropy cost by action type
    (let ((base-cost (case action-type
                       ((transform) 0.03)
                       ((synthesize) 0.05)
                       ((reason) 0.02)
                       ((optimize) 0.01)  ; Optimization reduces entropy
                       (else 0.02))))
      ;; Scale by current entropy
      (let ((entropy-factor (if entropy-state
                               (* entropy-state 0.01)
                               0.0)))
        (+ base-cost entropy-factor)))))

(define (qstar-complexity-cost state action)
  "Compute complexity cost component.
Based on system complexity and action complexity."
  (let ((entropy-state (qstar-get-property state 'total-entropy))
        (mem-state (qstar-get-property state 'total-memory-bytes))
        (action-type (list-ref action 1)))
    ;; Base complexity cost by action type
    (let ((base-cost (case action-type
                       ((transform) 0.2)
                       ((synthesize) 0.25)
                       ((reason) 0.15)
                       ((optimize) 0.1)  ; Optimization reduces complexity
                       (else 0.15))))
      ;; Scale by entropy and memory (complexity indicators)
      (let ((entropy-factor (if entropy-state
                               (* entropy-state 0.02)
                               0.0))
            (mem-factor (if mem-state
                          (* mem-state 0.0000005)
                          0.0)))
        (+ base-cost entropy-factor mem-factor)))))

(define (qstar-safety-penalty state action)
  "Compute safety penalty component.
Checks if action violates safety constraints via Prolog rules."
  (let ((action-type (list-ref action 1))
        (operator (list-ref action 2)))
    ;; Check safety constraints via Prolog
    ;; Query: (safe-action action-type operator)
    (let ((safety-check (prolog-query `(safe-action ,action-type ,operator))))
      (if (null? safety-check)
          ;; No safety rule allows this - apply penalty
          (case action-type
            ((transform) 0.5)  ; Transformations can be risky
            ((synthesize) 0.3)  ; Synthesis is moderate risk
            ((reason) 0.1)      ; Reasoning is low risk
            ((optimize) 0.2)    ; Optimization is moderate risk
            (else 0.3))
          ;; Action is safe - no penalty
          0.0))))

(define (qstar-apply-action state action)
  "Apply action to state, return next state.
Now integrates with action executor for actual execution."
  ;; Load action executor
  (load "../action/executor.scm")
  ;; Execute action
  (let* ((action-result (execute-action action))
         (success? (list-ref action-result 2))
         (outcome (list-ref action-result 3))
         (feedback (action-feedback action action-result))
         (history (store-action-history action action-result feedback)))
    ;; Update state based on action outcome
    ;; For now, return state with action outcome stored
    (let ((updated-properties (cons `((last-action . ,action)
                                     (last-action-result . ,action-result)
                                     (last-action-feedback . ,feedback))
                                   (list-ref state 3))))
      (list 'qstar-state
            (list-ref state 1)  ; Keep same UUID
            (list-ref state 2)  ; Keep same layers
            updated-properties))))

(define (qstar-goal-p state)
  "Check if state is a goal state.
Goal states have high consistency and low entropy."
  (let ((consistency (qstar-get-property state 'consistency-score))
        (entropy (qstar-get-property state 'total-entropy)))
    ;; Goal: high consistency (>= 0.8) and low entropy (<= 1.0)
    (and consistency
         entropy
         (>= consistency 0.8)
         (<= entropy 1.0))))

(define (qstar-future-value state)
  "Compute future value using dynamic programming or A*.
Uses A* search to estimate optimal future value from current state."
  ;; Load A* search
  (load "../qstar/a-star.scm")
  ;; Use A* to find path to goal, return negative of path cost
  (let ((goal-pred qstar-goal-p)
        (heuristic heuristic-euclidean)
        (opts '((max-nodes . 100))))
    (let ((result (qstar-a-star state goal-pred heuristic opts)))
      (let ((path-cost (list-ref result 1)))
        ;; Future value is negative of cost (lower cost = higher value)
        (- path-cost)))))

;; Policy Selection

(define (qstar-policy state action-space)
  "Select optimal action from action space.
STATE: current state
ACTION-SPACE: list of available actions
Returns (best-action value candidates)."
  (let* ((evaluations (map (lambda (action)
                              (let ((result (qstar-evaluate state action)))
                                (list action (list-ref result 0) result)))
                            action-space))
         (sorted (sort-evaluations evaluations))
         (best (car sorted)))
    (list (list-ref best 0)  ; best action
          (list-ref best 1)   ; value
          sorted)))           ; all candidates

(define (sort-evaluations evaluations)
  "Sort evaluations by Q-value (ascending = lower cost = better)."
  (let loop ((remaining evaluations)
             (sorted '()))
    (if (null? remaining)
        (reverse sorted)
        (let ((best (find-min-evaluation remaining)))
          (loop (remove best remaining)
                (cons best sorted))))))

(define (find-min-evaluation evaluations)
  "Find evaluation with minimum (best) Q-value."
  (let loop ((remaining (cdr evaluations))
             (best (car evaluations)))
    (if (null? remaining)
        best
        (let* ((current (car remaining))
               (best-value (list-ref best 1))
               (current-value (list-ref current 1)))
          (if (< current-value best-value)
              (loop (cdr remaining) current)
              (loop (cdr remaining) best))))))

(define (remove item list)
  "Remove first occurrence of item from list."
  (if (null? list)
      '()
      (if (equal? (car list) item)
          (cdr list)
          (cons (car list) (remove item (cdr list))))))

;; Q* API

(define (qstar-evaluate-action state-id action)
  "Evaluate Q* for action in state.
STATE-ID: URI or ID of state
ACTION: action representation
Returns Q* evaluation result."
  (let ((state (substrate-get-memory state-id)))
    (qstar-evaluate state action)))

(define (qstar-select-policy state-id action-space)
  "Select optimal policy from action space.
STATE-ID: URI or ID of state
ACTION-SPACE: list of actions
Returns policy result."
  (let ((state (substrate-get-memory state-id)))
    (qstar-policy state action-space)))

;; Functions are exported by default in R5RS

