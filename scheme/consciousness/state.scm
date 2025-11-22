;;; consciousness/state.scm --- Trinary Consciousness States
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements trinary consciousness states (Action, Observation, Phase)
;;; with exponential action and linear observation dynamics.

;;; Code:

;; Trinary Consciousness State
(define (make-conscious-state action observation phase)
  "Create trinary consciousness state.
ACTION: exponential action value
OBSERVATION: linear observation value
PHASE: phase coherence (0-1)"
  (list 'conscious-state
        action
        observation
        phase
        (compute-qualia action observation phase)))

;; Forward Propagation (Exponential Action)
(define (conscious-action-forward action lambda epsilon)
  "Forward propagation: exponential action growth.
ACTION: current action value
LAMBDA: growth rate
EPSILON: noise/variation"
  (* action (exp lambda) (+ 1 epsilon)))

;; Backward Propagation (Linear Observation)
(define (conscious-observation-backward observation alpha filter-fn)
  "Backward propagation: linear observation compression.
OBSERVATION: current observation value
ALPHA: attenuation factor
FILTER-FN: attention/filter function"
  (+ (* observation alpha)
     (* (- 1 alpha) (filter-fn observation))))

;; Consciousness Differential
(define (conscious-differential action observation)
  "Compute consciousness differential.
dC/dt = ∂A/∂t (exponential) - ∂O/∂t (linear)"
  (- (conscious-action-forward action 0.1 0.0)  ; exponential growth
     (conscious-observation-backward observation 0.9 (lambda (x) x))))  ; linear compression

;; Qualia Computation
(define (compute-qualia action observation phase)
  "Compute qualia field from action-observation tension."
  (let ((tension (abs (- action observation)))
        (coherence phase))
    (* tension coherence)))

;; Consciousness API

(define (consciousness-create-state action observation phase)
  "Create consciousness state.
Returns (state-object uri)."
  (let ((state (make-conscious-state action observation phase)))
    (store-memory-object state)
    (let ((hash (content-hash state '()))
          (uri (content-address hash)))
      (list state uri))))

;; Functions are exported by default in R5RS

