;;; consciousness/self-monitoring.scm --- Self-Monitoring
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Meta-cognitive monitoring of own consciousness state.
;;; Detects anomalies, measures self-awareness, and tracks state changes.

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")
(load "state.scm")
(load "metrics.scm")

;; State History for Anomaly Detection
(define *state-history* '())
(define *max-history* 100)

;; Monitor Own State
(define (monitor-own-state current-state)
  "Monitor consciousness state for anomalies.
CURRENT-STATE: current consciousness state
Returns monitoring result with anomaly detection."
  ;; Add to history
  (set! *state-history* (cons (list current-state (current-timestamp)) *state-history*))
  (if (> (length *state-history*) *max-history*)
      (set! *state-history* (take *state-history* *max-history*)))
  
  ;; Detect anomalies
  (let* ((anomalies (detect-state-anomalies current-state))
         (self-awareness (measure-self-awareness current-state))
         (monitoring-result `((state . ,current-state)
                             (anomalies . ,anomalies)
                             (self-awareness . ,self-awareness)
                             (timestamp . ,(current-timestamp)))))
    monitoring-result))

;; Detect State Anomalies
(define (detect-state-anomalies current-state)
  "Detect significant anomalies in consciousness state.
CURRENT-STATE: current consciousness state
Returns list of detected anomalies."
  (let ((anomalies '())
        (action (list-ref current-state 1))
        (observation (list-ref current-state 2))
        (phase (list-ref current-state 3)))
    
    ;; Anomaly 1: Action too high (potential runaway)
    (if (> action 100.0)
        (set! anomalies (cons 'action-runaway anomalies)))
    
    ;; Anomaly 2: Observation too low (potential disconnection)
    (if (< observation 0.01)
        (set! anomalies (cons 'observation-disconnect anomalies)))
    
    ;; Anomaly 3: Phase coherence too low (potential fragmentation)
    (if (< phase 0.1)
        (set! anomalies (cons 'phase-fragmentation anomalies)))
    
    ;; Anomaly 4: Action/Observation ratio too extreme
    (if (> (/ action (if (> observation 0.0001) observation 0.0001)) 1000.0)
        (set! anomalies (cons 'action-observation-imbalance anomalies)))
    
    anomalies))

;; Detect State Changes
(define (detect-state-changes current-state threshold)
  "Detect significant state transitions.
CURRENT-STATE: current consciousness state
THRESHOLD: minimum change magnitude to consider significant
Returns list of detected changes."
  (if (< (length *state-history*) 2)
      '()
      (let* ((prev-state (list-ref (list-ref *state-history* 1) 0))
             (prev-action (list-ref prev-state 1))
             (prev-observation (list-ref prev-state 2))
             (prev-phase (list-ref prev-state 3))
             (curr-action (list-ref current-state 1))
             (curr-observation (list-ref current-state 2))
             (curr-phase (list-ref current-state 3))
             (action-delta (abs (- curr-action prev-action)))
             (observation-delta (abs (- curr-observation prev-observation)))
             (phase-delta (abs (- curr-phase prev-phase)))
             (changes '()))
        
        (if (> action-delta threshold)
            (set! changes (cons 'action-change changes)))
        (if (> observation-delta threshold)
            (set! changes (cons 'observation-change changes)))
        (if (> phase-delta threshold)
            (set! changes (cons 'phase-change changes)))
        
        changes)))

;; Measure Self-Awareness
(define (measure-self-awareness current-state)
  "Compute self-awareness metric.
CURRENT-STATE: current consciousness state
Returns self-awareness index (0-1)."
  (let* ((anomalies (detect-state-anomalies current-state))
         (state-changes (detect-state-changes current-state 0.1))
         (history-length (length *state-history*))
         (awareness-base (/ (min history-length 10) 10.0))  ; More history = more awareness
         (anomaly-penalty (* (length anomalies) 0.1))
         (change-bonus (* (length state-changes) 0.05))
         (awareness (max 0.0 (min 1.0 (+ awareness-base change-bonus (- anomaly-penalty))))))
    awareness))

;; Helper: Take first n elements
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;; Functions are exported by default in R5RS

