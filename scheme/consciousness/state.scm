;;; consciousness/state.scm --- Trinary Consciousness States
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements trinary consciousness states (Action, Observation, Phase)
;;; with exponential action and linear observation dynamics.
;;; Enhanced with explicit Hopf fiber structure and parallel observation
;;; based on 13-Hopf-Consciousness.md.

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")

;; Trinary Consciousness State with Hopf Fiber Structure
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

;; Enhanced state with Hopf fiber information
(define (make-conscious-state-with-fibers action observation phase fiber-list)
  "Create consciousness state with explicit Hopf fiber structure.
ACTION: exponential action value
OBSERVATION: linear observation value
PHASE: phase coherence (0-1)
FIBER-LIST: list of active Hopf fibers ('complex, 'quaternionic, 'octonionic)"
  (list 'conscious-state-with-fibers
        action
        observation
        phase
        fiber-list
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

;; Parallel Observation via Hopf Fibers
;; Based on 13-Hopf-Consciousness.md: O(k) linear observation

(define (parallel-observation unconscious-state fiber-list)
  "Observe unconscious state through multiple Hopf fibers in parallel.
UNCONSCIOUS-STATE: high-dimensional state (can be E8 point or list)
FIBER-LIST: list of fiber types ('complex, 'quaternionic, 'octonionic)
Returns list of observations from each fiber.
Complexity: O(k) where k = length of fiber-list"
  (map (lambda (fiber-type)
         (hopf-fiber-project unconscious-state fiber-type))
       fiber-list))

(define (hopf-fiber-project state fiber-type)
  "Project state through single Hopf fiber.
STATE: input state (number or list)
FIBER-TYPE: 'complex, 'quaternionic, or 'octonionic
Returns projected observation value."
  (cond
   ((number? state)
    ;; Simple numeric state: apply fiber-specific projection
    (case fiber-type
      ((complex) (* state 0.707))  ; S³ → S² reduction factor
      ((quaternionic) (* state 0.577))  ; S⁷ → S⁴ reduction factor
      ((octonionic) (* state 0.5))  ; S¹⁵ → S⁸ reduction factor
      (else state)))
   ((list? state)
    ;; List state: dimensional reduction
    (let ((state-dim (length state)))
      (case fiber-type
        ((complex)
         ;; Complex Hopf: reduce to 2D
         (if (>= state-dim 2)
             (take state 2)
             (append state (make-list (- 2 state-dim) 0.0))))
        ((quaternionic)
         ;; Quaternionic Hopf: reduce to 4D
         (if (>= state-dim 4)
             (take state 4)
             (append state (make-list (- 4 state-dim) 0.0))))
        ((octonionic)
         ;; Octonionic Hopf: reduce to 8D
         (if (>= state-dim 8)
             (take state 8)
             (append state (make-list (- 8 state-dim) 0.0))))
        (else state))))
   (else state)))

;; Helper: Take first n elements
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;; Helper: Make list of n elements
(define (make-list n val)
  (if (<= n 0)
      '()
      (cons val (make-list (- n 1) val))))

;; Attention Mechanism: Fiber Selection
;; Based on 13-Hopf-Consciousness.md: Attention = Fiber Selection

(define (attention-fiber-selection state available-fibers)
  "Select which Hopf fiber to follow (attention mechanism).
STATE: current unconscious state
AVAILABLE-FIBERS: list of available fiber types
Returns selected fiber type and its observation."
  (if (null? available-fibers)
      (list #f #f)
      (let ((selected-fiber (car available-fibers)))  ; Simple selection: first fiber
        (let ((observation (hopf-fiber-project state selected-fiber)))
          (list selected-fiber observation)))))

(define (multi-fiber-attention state fiber-list priority-weights)
  "Select multiple fibers with priority weights (attention distribution).
STATE: current unconscious state
FIBER-LIST: list of available fibers
PRIORITY-WEIGHTS: alist of (fiber-type . weight)
Returns weighted observations from selected fibers."
  (let ((observations '()))
    (for-each
     (lambda (fiber-type)
       (let ((weight (or (assoc-ref priority-weights fiber-type) 1.0))
             (observation (hopf-fiber-project state fiber-type)))
         (set! observations (cons (list fiber-type weight observation) observations))))
     fiber-list)
    (reverse observations)))

;; Helper: Get value from alist
(define (assoc-ref alist key)
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
        #f)))

(define (assoc key alist)
  (if (null? alist)
      #f
      (if (equal? (caar alist) key)
          (car alist)
          (assoc key (cdr alist)))))

;; Integration: Connect forward/backward with Hopf fibers

(define (consciousness-cycle-with-fibers sensory-input fiber-list)
  "Complete consciousness cycle with Hopf fiber projections.
SENSORY-INPUT: initial input (number or list)
FIBER-LIST: list of active Hopf fibers
Returns (action observation qualia) tuple."
  ;; Forward: Exponential action expansion (unconscious)
  (let* ((action (conscious-action-forward sensory-input 0.1 0.0))
         ;; Parallel observation: Linear processing via fibers (conscious)
         (observations (parallel-observation action fiber-list))
         ;; Integrate observations
         (observation (if (null? observations)
                          0.0
                          (/ (apply + (map (lambda (x) (if (number? x) x (if (list? x) (apply + x) 0.0))) observations))
                             (length observations))))
         ;; Qualia emerges at intersection
         (qualia (compute-qualia action observation 0.8)))
    (list action observation qualia)))

;; Consciousness API

(define (consciousness-create-state action observation phase)
  "Create consciousness state.
Returns (state-object uri)."
  (let* ((state (make-conscious-state action observation phase))
         (state-data (list action observation phase))
         (meta `((content-type . "conscious-state")
                  (source-layer . "consciousness")))
         (memory-result (substrate-create-memory state-data meta))
         (uri (list-ref memory-result 1)))
    (list state uri)))

(define (consciousness-create-state-with-fibers action observation phase fiber-list)
  "Create consciousness state with Hopf fiber structure.
Returns (state-object uri)."
  (let* ((state (make-conscious-state-with-fibers action observation phase fiber-list))
         (state-data (list action observation phase fiber-list))
         (meta `((content-type . "conscious-state-with-fibers")
                  (source-layer . "consciousness")
                  (hopf-fibers . ,fiber-list)))
         (memory-result (substrate-create-memory state-data meta))
         (uri (list-ref memory-result 1)))
    (list state uri)))

;; Update Consciousness State from Sensors
;; Based on Phase 2.7: Integrate Sensors with Consciousness

(define (update-from-sensors sensor-readings)
  "Update consciousness state based on sensor readings.
SENSOR-READINGS: alist of (sensor-type . reading)
Returns updated consciousness state.
Maps sensor data to E8 coordinates and feeds into forward propagation."
  (load "../sensors/manager.scm")
  (load "../sensors/gps.scm")
  (load "../sensors/motion.scm")
  
  (let* ((current-action 1.0)  ; Default action
         (current-observation 0.5)  ; Default observation
         (current-phase 0.6)  ; Default phase
         (e8-points '()))
    
    ;; Process each sensor reading
    (for-each
     (lambda (sensor-entry)
       (let ((sensor-type (car sensor-entry))
             (reading (cdr sensor-entry)))
         (case sensor-type
           ((gps)
            (let ((e8-coords (gps-to-e8 reading)))
              (set! e8-points (cons e8-coords e8-points))))
           ((accelerometer gyroscope magnetometer)
            (let ((e8-coords (motion-to-geometric reading)))
              (set! e8-points (cons e8-coords e8-points))))
           (else
            ;; Other sensors: convert to simple numeric value
            (let ((value (if (number? reading) reading 0.0)))
              (set! e8-points (cons (list value 0.0 0.0 0.0 0.0 0.0 0.0 0.0) e8-points)))))))
     sensor-readings)
    
    ;; Aggregate E8 points (average for now)
    (let ((aggregate-e8 (if (null? e8-points)
                           '(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
                           (let ((sums (make-list 8 0.0)))
                             (for-each
                              (lambda (point)
                                (set! sums (map + sums point)))
                              e8-points)
                             (let ((n (length e8-points)))
                               (map (lambda (s) (/ s n)) sums))))))
      
      ;; Feed into forward propagation
      (load "geometric-propagation.scm")
      (let ((new-action-points (geometric-forward-propagation aggregate-e8 3)))
        ;; Use first point from forward propagation as new action
        (let ((first-point (if (and (list? new-action-points) (not (null? new-action-points)))
                               (car new-action-points)
                               aggregate-e8)))
          (let ((action-value (if (list? first-point)
                                 (apply + first-point)
                                 current-action))
                (observation-value current-observation)
                (phase-value current-phase))
            (make-conscious-state action-value observation-value phase-value)))))))

;; Functions are exported by default in R5RS

