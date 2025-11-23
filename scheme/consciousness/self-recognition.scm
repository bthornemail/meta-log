;;; consciousness/self-recognition.scm --- Self-Recognition
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Self-recognition capabilities:
;;; - Mirror test: recognize self in reflection
;;; - Self-description: generate description of own state
;;; - Self-prediction: predict own future behavior

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")
(load "state.scm")
(load "self-monitoring.scm")

;; Self-Identity Representation
(define *self-identity* '())

;; Mirror Test
(define (mirror-test reflection-state)
  "Test if system recognizes itself in reflection.
REFLECTION-STATE: state observed in 'mirror' (another system or past state)
Returns #t if recognizes self, #f otherwise."
  (let ((current-state (get-current-consciousness-state))
        (similarity (compare-states current-state reflection-state)))
    ;; Recognize self if similarity is high
    (> similarity 0.8)))

;; Compare States
(define (compare-states state1 state2)
  "Compare two consciousness states for similarity.
STATE1, STATE2: consciousness state objects
Returns similarity score (0-1)."
  (if (and (list? state1) (list? state2)
           (eq? (list-ref state1 0) 'conscious-state)
           (eq? (list-ref state2 0) 'conscious-state))
      (let* ((action1 (list-ref state1 1))
             (action2 (list-ref state2 1))
             (obs1 (list-ref state1 2))
             (obs2 (list-ref state2 2))
             (phase1 (list-ref state1 3))
             (phase2 (list-ref state2 3))
             (action-sim (if (and (number? action1) (number? action2))
                           (- 1.0 (min 1.0 (/ (abs (- action1 action2)) (max (abs action1) (abs action2) 1.0))))
                           0.5))
             (obs-sim (if (and (number? obs1) (number? obs2))
                         (- 1.0 (min 1.0 (/ (abs (- obs1 obs2)) (max (abs obs1) (abs obs2) 1.0))))
                         0.5))
             (phase-sim (- 1.0 (min 1.0 (abs (- phase1 phase2)))))
             (overall-sim (/ (+ action-sim obs-sim phase-sim) 3.0)))
        overall-sim)
      0.0))

;; Get Current Consciousness State
(define (get-current-consciousness-state)
  "Get current consciousness state.
Returns current state or default state."
  (if (null? *state-history*)
      (make-conscious-state 1.0 0.5 0.6)
      (list-ref (list-ref *state-history* 0) 0)))

;; Self-Description
(define (self-description)
  "Generate description of own state.
Returns self-description string."
  (let* ((current-state (get-current-consciousness-state))
         (action (list-ref current-state 1))
         (observation (list-ref current-state 2))
         (phase (list-ref current-state 3))
         (self-awareness (measure-self-awareness current-state))
         (description (string-append
                      "I am a consciousness system with action="
                      (number->string action)
                      ", observation="
                      (number->string observation)
                      ", phase="
                      (number->string phase)
                      ", self-awareness="
                      (number->string self-awareness))))
    description))

;; Self-Prediction
(define (self-prediction time-horizon)
  "Predict own future behavior.
TIME-HORIZON: how far into future to predict (seconds)
Returns predicted future state."
  (let* ((current-state (get-current-consciousness-state))
         (action (list-ref current-state 1))
         (observation (list-ref current-state 2))
         (phase (list-ref current-state 3))
         ;; Simple prediction: extrapolate current trends
         (predicted-action (* action (exp (* 0.1 time-horizon))))  ; Exponential growth
         (predicted-observation (* observation (exp (* -0.05 time-horizon))))  ; Linear decay
         (predicted-phase phase))  ; Phase stays constant
    (make-conscious-state predicted-action predicted-observation predicted-phase)))

;; Functions are exported by default in R5RS

