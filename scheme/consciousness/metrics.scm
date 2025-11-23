;;; consciousness/metrics.scm --- Consciousness Quality Metrics
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements consciousness quality metrics for monitoring and optimization.
;;; Metrics track creativity, focus, coherence, qualia richness, and learning.

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")
(load "../consciousness/state.scm")
(load "../consciousness/qualia.scm")

;; Consciousness Metrics Structure
(define (make-consciousness-metrics creativity focus coherence richness learning-velocity)
  "Create consciousness metrics object.
CREATIVITY: creativity index (exponential growth rate)
FOCUS: focus efficiency (linear selection ratio)
COHERENCE: coherence stability (phase variance)
RICHNESS: qualia richness (diversity measure)
LEARNING-VELOCITY: learning velocity (adaptation rate)
Returns metrics object."
  (list 'consciousness-metrics
        (uuid-generate)
        creativity
        focus
        coherence
        richness
        learning-velocity
        `((created-at . ,(current-timestamp))
          (version . 1))))

;; Creativity Index (Exponential Growth Rate)
(define (compute-creativity-index current-action previous-action)
  "Compute creativity index from action growth.
CURRENT-ACTION: action value at time t
PREVIOUS-ACTION: action value at time t-1
Returns creativity index (A_t / A_{t-1})."
  (if (and (> previous-action 0.0) (> current-action 0.0))
      (/ current-action previous-action)
      1.0))

;; Focus Efficiency (Linear Selection Ratio)
(define (compute-focus-efficiency selected-observations total-observations)
  "Compute focus efficiency from observation selection.
SELECTED-OBSERVATIONS: number of observations selected
TOTAL-OBSERVATIONS: total number of observations available
Returns focus efficiency (O_selected / O_total)."
  (if (> total-observations 0)
      (/ selected-observations total-observations)
      0.0))

;; Coherence Stability (Phase Variance)
(define (compute-coherence-stability phase-history)
  "Compute coherence stability from phase history.
PHASE-HISTORY: list of phase values over time
Returns stability measure (1 - variance)."
  (if (< (length phase-history) 2)
      1.0
      (let* ((mean-val (compute-mean phase-history))
             (variance (compute-variance phase-history mean-val)))
        (max 0.0 (- 1.0 variance)))))

(define (compute-mean values)
  "Compute mean of values."
  (if (null? values)
      0.0
      (/ (apply + values) (length values))))

(define (compute-variance values mean-val)
  "Compute variance of values."
  (if (null? values)
      0.0
      (let ((squared-diffs (map (lambda (x) (* (- x mean-val) (- x mean-val))) values)))
        (/ (apply + squared-diffs) (length values)))))

;; Qualia Richness (Diversity Measure)
(define (compute-qualia-richness qualia-field)
  "Compute qualia richness from qualia field.
QUALIA-FIELD: qualia field object
Returns richness value (0-1)."
  (if (list? qualia-field)
      (list-ref qualia-field 4)  ; richness is 5th element
      0.0))

;; Learning Velocity (Adaptation Rate)
(define (compute-learning-velocity current-optimal previous-optimal time-delta)
  "Compute learning velocity from optimal observation changes.
CURRENT-OPTIMAL: optimal observation at time t
PREVIOUS-OPTIMAL: optimal observation at time t-1
TIME-DELTA: time difference
Returns learning velocity (d(O_optimal)/dt)."
  (if (and (> time-delta 0.0) (number? current-optimal) (number? previous-optimal))
      (/ (- current-optimal previous-optimal) time-delta)
      0.0))

;; Consciousness Quality Metric (CQM)
(define (compute-cqm metrics weights)
  "Compute overall Consciousness Quality Metric.
METRICS: consciousness metrics object
WEIGHTS: alist of (metric-name . weight)
Returns CQM value."
  (let ((creativity (list-ref metrics 2))
        (focus (list-ref metrics 3))
        (coherence (list-ref metrics 4))
        (richness (list-ref metrics 5))
        (learning (list-ref metrics 6))
        (w-action (or (assoc-ref weights 'action) 1.0))
        (w-obs (or (assoc-ref weights 'observation) 1.0))
        (w-coherence (or (assoc-ref weights 'coherence) 1.0))
        (w-qualia (or (assoc-ref weights 'qualia) 1.0))
        (w-learning (or (assoc-ref weights 'learning) 1.0)))
    (+ (* w-action creativity)
       (* w-obs focus)
       (* w-coherence coherence)
       (* w-qualia richness)
       (* w-learning learning))))

;; Metrics Collection from Conscious State
(define (collect-metrics conscious-state previous-state qualia-field)
  "Collect all consciousness metrics from state.
CONSCIOUS-STATE: current conscious state
PREVIOUS-STATE: previous conscious state (or #f)
QUALIA-FIELD: current qualia field (or #f)
Returns metrics object."
  (let* ((current-action (list-ref conscious-state 1))
         (current-observation (list-ref conscious-state 2))
         (current-phase (list-ref conscious-state 3))
         (previous-action (if previous-state (list-ref previous-state 1) current-action))
         (previous-observation (if previous-state (list-ref previous-state 2) current-observation))
         (creativity (compute-creativity-index current-action previous-action))
         (focus (compute-focus-efficiency current-observation 1.0))  ; Simplified
         (coherence current-phase)  ; Use phase as coherence measure
         (richness (if qualia-field
                       (compute-qualia-richness qualia-field)
                       0.0))
         (learning (compute-learning-velocity current-observation previous-observation 1.0)))
    (make-consciousness-metrics creativity focus coherence richness learning)))

;; Metrics History Tracking
(define *metrics-history* '())

(define (record-metrics metrics)
  "Record metrics in history.
METRICS: metrics object
Returns updated history."
  (set! *metrics-history* (cons metrics *metrics-history*))
  *metrics-history*)

(define (get-metrics-history limit)
  "Get recent metrics history.
LIMIT: maximum number of entries to return
Returns list of metrics objects."
  (let ((count (min limit (length *metrics-history*))))
    (reverse (take *metrics-history* count))))

(define (take list n)
  "Take first n elements from list."
  (let loop ((remaining list) (i 0) (result '()))
    (if (or (null? remaining) (>= i n))
        (reverse result)
        (loop (cdr remaining) (+ i 1) (cons (car remaining) result)))))

;; Functions are exported by default in R5RS

