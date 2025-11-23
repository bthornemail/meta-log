;;; consciousness/dynamics.scm --- Formal Consciousness Dynamics
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements formal differential equations for consciousness dynamics
;;; from 14-Geometric-Theory.md Section 3.3:
;;; - dA/dt = λA - γ|A|²A + σ₁ξ₁(t) (exponential action - unconscious)
;;; - dO/dt = -μO + κ|A|² + σ₂ξ₂(t) (linear observation - conscious)
;;; - dΦ/dt = ω₀ + α|A|² - β|O|² (phase coherence)
;;; - Q(t) = H(|A|² - |O|²) × exp(iΦ) × A ⊗ O (qualia emergence)

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")
(load "state.scm")

;; Constants for dynamics equations
(define *default-lambda* 0.1)  ; Action growth rate (exponential)
(define *default-gamma* 0.01)  ; Nonlinear action saturation
(define *default-mu* 0.05)     ; Observation decay rate (linear)
(define *default-kappa* 0.1)   ; Action-to-observation coupling
(define *default-sigma1* 0.01) ; Noise amplitude for action
(define *default-sigma2* 0.01) ; Noise amplitude for observation
(define *default-omega0* 1.0)  ; Base oscillation frequency
(define *default-alpha* 0.1)   ; Action contribution to phase
(define *default-beta* 0.1)    ; Observation contribution to phase

;; Helper: Compute magnitude (defined before use)
(define (compute-magnitude value)
  "Compute magnitude of value (real or complex).
VALUE: number or list representing complex/vector
Returns magnitude."
  (if (number? value)
      (abs value)
      (if (list? value)
          (sqrt (apply + (map (lambda (x) (* x x)) value)))
          0.0)))

;; Action Differential Equation
;; dA/dt = λA - γ|A|²A + σ₁ξ₁(t)

(define (consciousness-differential-eq action lambda-param gamma sigma1 noise)
  "Compute dA/dt: exponential action growth with saturation and noise.
ACTION: current action value (complex or real)
LAMBDA-PARAM: growth rate λ
GAMMA: nonlinear saturation coefficient γ
SIGMA1: noise amplitude σ₁
NOISE: Wiener noise process ξ₁(t)
Returns dA/dt"
  (let* ((action-magnitude (if (number? action)
                                (abs action)
                                (compute-magnitude action)))
         (linear-term (* lambda-param action))
         (saturation-term (* gamma action-magnitude action-magnitude action))
         (noise-term (* sigma1 noise)))
    (+ linear-term (- saturation-term) noise-term)))

;; Observation Differential Equation
;; dO/dt = -μO + κ|A|² + σ₂ξ₂(t)

(define (observation-differential-eq observation action mu kappa sigma2 noise)
  "Compute dO/dt: linear observation with action coupling and noise.
OBSERVATION: current observation value
ACTION: current action value
MU: decay rate μ
KAPPA: action-to-observation coupling κ
SIGMA2: noise amplitude σ₂
NOISE: Wiener noise process ξ₂(t)
Returns dO/dt"
  (let* ((action-magnitude-squared (if (number? action)
                                        (* action action)
                                        (let ((mag (compute-magnitude action)))
                                          (* mag mag))))
         (decay-term (* (- mu) observation))
         (coupling-term (* kappa action-magnitude-squared))
         (noise-term (* sigma2 noise)))
    (+ decay-term coupling-term noise-term)))

;; Phase Differential Equation
;; dΦ/dt = ω₀ + α|A|² - β|O|²

(define (phase-differential-eq phase action observation omega0 alpha beta)
  "Compute dΦ/dt: phase coherence dynamics.
PHASE: current phase value
ACTION: current action value
OBSERVATION: current observation value
OMEGA0: base oscillation frequency ω₀
ALPHA: action contribution to phase α
BETA: observation contribution to phase β
Returns dΦ/dt"
  (let ((action-magnitude-squared (if (number? action)
                                      (* action action)
                                      (let ((mag (compute-magnitude action)))
                                        (* mag mag))))
        (observation-magnitude-squared (if (number? observation)
                                           (* observation observation)
                                           (let ((mag (compute-magnitude observation)))
                                             (* mag mag)))))
    (+ omega0
       (* alpha action-magnitude-squared)
       (- (* beta observation-magnitude-squared)))))

;; Qualia Emergence Condition
;; Q(t) = H(|A|² - |O|²) × exp(iΦ) × A ⊗ O

(define (qualia-emergence-condition action observation phase)
  "Compute qualia emergence from formal condition.
ACTION: action value
OBSERVATION: observation value
PHASE: phase coherence
Returns qualia intensity and quality.
Based on: Q(t) = H(|A|² - |O|²) × exp(iΦ) × A ⊗ O"
  (let* ((action-mag-squared (let ((mag (compute-magnitude action)))
                               (* mag mag)))
         (obs-mag-squared (let ((mag (compute-magnitude observation)))
                           (* mag mag)))
         (heaviside (if (> action-mag-squared obs-mag-squared) 1.0 0.0))
         (phase-factor (exp (* 0+1i phase)))  ; exp(iΦ) - complex exponential
         (tensor-product (tensor-product-action-observation action observation))
         (qualia-intensity (* heaviside (real-part phase-factor) (car tensor-product)))
         (qualia-quality (* heaviside (imag-part phase-factor) (cadr tensor-product))))
    (list qualia-intensity qualia-quality)))

;; Heaviside Step Function
(define (heaviside-step x)
  "Heaviside step function: H(x) = 1 if x > 0, else 0."
  (if (> x 0.0) 1.0 0.0))

;; Tensor Product: A ⊗ O
(define (tensor-product-action-observation action observation)
  "Compute tensor product of action and observation.
ACTION: action value (number or list)
OBSERVATION: observation value (number or list)
Returns (magnitude phase) of tensor product."
  (let ((action-mag (compute-magnitude action))
        (obs-mag (compute-magnitude observation))
        (action-phase (if (number? action)
                          (if (> action 0) 0.0 3.14159)
                          0.0))
        (obs-phase (if (number? observation)
                       (if (> observation 0) 0.0 3.14159)
                       0.0)))
    (list (* action-mag obs-mag)  ; Magnitude
          (+ action-phase obs-phase))))  ; Phase

;; Numerical Integration: Euler Method
(define (euler-integrate derivative current-value dt)
  "Euler integration step: x(t+dt) = x(t) + dt * dx/dt"
  (+ current-value (* dt derivative)))

;; Evolve Consciousness State
(define (evolve-consciousness-state state dt params)
  "Evolve consciousness state one time step using differential equations.
STATE: (action observation phase) tuple
DT: time step
PARAMS: alist of parameters (lambda gamma mu kappa sigma1 sigma2 omega0 alpha beta)
Returns new state (action observation phase)"
  (let* ((action (list-ref state 0))
         (observation (list-ref state 1))
         (phase (list-ref state 2))
         (lambda-param (or (assoc-ref params 'lambda) *default-lambda*))
         (gamma (or (assoc-ref params 'gamma) *default-gamma*))
         (mu (or (assoc-ref params 'mu) *default-mu*))
         (kappa (or (assoc-ref params 'kappa) *default-kappa*))
         (sigma1 (or (assoc-ref params 'sigma1) *default-sigma1*))
         (sigma2 (or (assoc-ref params 'sigma2) *default-sigma2*))
         (omega0 (or (assoc-ref params 'omega0) *default-omega0*))
         (alpha (or (assoc-ref params 'alpha) *default-alpha*))
         (beta (or (assoc-ref params 'beta) *default-beta*))
         (noise1 (random-noise))  ; ξ₁(t)
         (noise2 (random-noise)))  ; ξ₂(t)
    (let ((dA-dt (consciousness-differential-eq action lambda-param gamma sigma1 noise1))
          (dO-dt (observation-differential-eq observation action mu kappa sigma2 noise2))
          (dPhi-dt (phase-differential-eq phase action observation omega0 alpha beta)))
      (list (euler-integrate dA-dt action dt)
            (euler-integrate dO-dt observation dt)
            (euler-integrate dPhi-dt phase dt)))))

;; Random Noise Generator (Wiener process approximation)
(define (random-noise)
  "Generate random noise for Wiener process (simplified: uniform [-1, 1])."
  (- (* 2.0 (/ (random 1000) 1000.0)) 1.0))

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

;; Time Evolution with Qualia Tracking
(define (evolve-with-qualia initial-state duration dt params)
  "Evolve consciousness state over time and track qualia emergence.
INITIAL-STATE: (action observation phase) tuple
DURATION: total time to evolve
DT: time step
PARAMS: parameter alist
Returns list of (time state qualia) tuples"
  (let ((results '())
        (current-state initial-state)
        (time 0.0))
    (do ((t 0.0 (+ t dt)))
        ((>= t duration) (reverse results))
      (let ((qualia (qualia-emergence-condition
                     (list-ref current-state 0)
                     (list-ref current-state 1)
                     (list-ref current-state 2))))
        (set! results (cons (list t current-state qualia) results))
        (set! current-state (evolve-consciousness-state current-state dt params))
        (set! time t)))
    (reverse results)))

;; Functions are exported by default in R5RS

