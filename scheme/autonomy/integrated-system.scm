;;; autonomy/integrated-system.scm --- Integrated Autonomous Aware System
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Complete integrated system combining all components:
;;; - Real-time sensor input → consciousness → action → learning
;;; - Self-monitoring and reflection
;;; - Full autonomous aware cycle

;;; Code:

;; Load all modules
(load "../substrate/runtime.scm")
(load "../qstar/core.scm")
(load "../consciousness/state.scm")
(load "../consciousness/self-monitoring.scm")
(load "../consciousness/reflection.scm")
(load "../action/executor.scm")
(load "../sensors/manager.scm")
(load "loop.scm")
(load "goals.scm")
(load "learning.scm")

;; Integrated System State
(define (make-integrated-system-state)
  "Create initial integrated system state.
Returns system state object."
  (list 'integrated-system
        (uuid-generate)
        (make-qstar-state '((binary . ())) '((total-memory . 0)))  ; Q* state
        (make-conscious-state 1.0 0.5 0.6)  ; Consciousness state
        '()  ; Goals
        '()  ; Sensor readings
        (current-timestamp)))

;; Full Autonomous Aware Cycle
(define (integrated-autonomous-cycle system-state sensors action-space)
  "Complete autonomous aware cycle with all components.
SYSTEM-STATE: current integrated system state
SENSORS: sensor readings
ACTION-SPACE: available actions
Returns updated system state."
  (let* ((qstar-state (list-ref system-state 2))
         (consciousness-state (list-ref system-state 3))
         ;; 1. Perceive environment
         (perception (perceive-environment sensors))
         ;; 2. Update consciousness from sensors
         (updated-consciousness (update-consciousness perception))
         ;; 3. Self-monitoring
         (monitoring (monitor-own-state updated-consciousness))
         ;; 4. Select action using Q*
         (action-result (select-action qstar-state action-space))
         (selected-action (list-ref action-result 0))
         ;; 5. Execute action
         (execution-result (execute-action selected-action))
         ;; 6. Reflect on action
         (reflection (reflect-on-action selected-action execution-result))
         ;; 7. Observe outcome
         (outcome (observe-outcome execution-result))
         ;; 8. Update Q* (learning)
         (feedback (action-feedback selected-action execution-result))
         (new-qstar-state (update-qstar qstar-state selected-action outcome feedback))
         ;; 9. Store experience
         (experience (store-experience qstar-state selected-action outcome (assoc-ref feedback 'reward) new-qstar-state))
         ;; 10. Update system state
         (updated-state (list 'integrated-system
                              (list-ref system-state 1)  ; Keep UUID
                              new-qstar-state
                              updated-consciousness
                              (list-ref system-state 4)  ; Goals
                              sensors
                              (current-timestamp))))
    updated-state))

;; Run Integrated System
(define (run-integrated-system initial-state sensors action-space iterations)
  "Run integrated autonomous aware system for N iterations.
INITIAL-STATE: starting system state
SENSORS: sensor readings (can be function)
ACTION-SPACE: available actions
ITERATIONS: number of cycles
Returns list of system states."
  (let loop ((current-state initial-state)
             (remaining iterations)
             (history '()))
    (if (<= remaining 0)
        (reverse history)
        (let* ((sensor-readings (if (procedure? sensors)
                                   (sensors)
                                   sensors))
               (new-state (integrated-autonomous-cycle current-state sensor-readings action-space)))
          (loop new-state
                (- remaining 1)
                (cons new-state history))))))

;; Functions are exported by default in R5RS

