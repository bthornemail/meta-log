;;; autonomy/learning.scm --- Reinforcement Learning
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Reinforcement learning from action outcomes.
;;; Updates Q* values, stores experiences, and replays past experiences.

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")
(load "../qstar/core.scm")
(load "../action/executor.scm")

;; Experience Storage
(define *experience-buffer* '())
(define *max-experiences* 1000)  ; Maximum stored experiences

;; Experience Representation
(define (make-experience state action outcome reward next-state)
  "Create experience tuple for learning.
STATE: Q* state before action
ACTION: action taken
OUTCOME: action outcome
REWARD: reward value
NEXT-STATE: Q* state after action
Returns experience object."
  (list 'experience
        state
        action
        outcome
        reward
        next-state
        (current-timestamp)))

;; Store Experience
(define (store-experience state action outcome reward next-state)
  "Store (state, action, outcome) tuple for learning.
STATE: Q* state before action
ACTION: action taken
OUTCOME: action outcome
REWARD: reward value
NEXT-STATE: Q* state after action
Returns experience object."
  (let ((experience (make-experience state action outcome reward next-state)))
    (set! *experience-buffer* (cons experience *experience-buffer*))
    ;; Limit buffer size
    (if (> (length *experience-buffer*) *max-experiences*)
        (set! *experience-buffer* (take *experience-buffer* *max-experiences*)))
    experience))

;; Update Q* Values
(define (update-q-values state action reward next-state gamma alpha)
  "Update Q* values from action outcomes (Q-learning).
STATE: Q* state before action
ACTION: action taken
REWARD: reward value
NEXT-STATE: Q* state after action
GAMMA: discount factor (typically 0.95)
ALPHA: learning rate (typically 0.1)
Returns updated Q-value estimate."
  (let* ((current-q (qstar-evaluate state action))
         (current-value (list-ref current-q 0))
         (next-best-value (qstar-future-value next-state))
         (target-q (+ reward (* gamma next-best-value)))
         (new-q (+ current-value (* alpha (- target-q current-value)))))
    new-q))

;; Replay Experience
(define (replay-experience experience gamma alpha)
  "Learn from past experience.
EXPERIENCE: experience object
GAMMA: discount factor
ALPHA: learning rate
Returns updated Q-value."
  (let ((state (list-ref experience 1))
        (action (list-ref experience 2))
        (reward (list-ref experience 4))
        (next-state (list-ref experience 5)))
    (update-q-values state action reward next-state gamma alpha)))

;; Batch Replay
(define (replay-batch experiences gamma alpha)
  "Learn from multiple past experiences.
EXPERIENCES: list of experience objects
GAMMA: discount factor
ALPHA: learning rate
Returns list of updated Q-values."
  (map (lambda (exp) (replay-experience exp gamma alpha)) experiences))

;; Integration with Substrate for Persistent Learning
(define (save-learning-state)
  "Save learning state (experiences, Q-values) to substrate.
Returns (memory-object uri)."
  (let* ((data `((experiences . ,*experience-buffer*)
                (experience-count . ,(length *experience-buffer*))))
         (meta `((content-type . "learning-state")
                (source-layer . "autonomy")
                (timestamp . ,(current-timestamp))))
         (memory (substrate-create-memory data meta))
         (uri (list-ref memory 1)))
    (list memory uri)))

;; Load Learning State
(define (load-learning-state uri)
  "Load learning state from substrate.
URI: content address of learning state
Returns #t if loaded, #f if error."
  (let ((memory-result (substrate-get-memory uri)))
    (if (not memory-result)
        #f
        (let ((data (list-ref memory-result 0)))
          (let ((experiences (assoc-ref data 'experiences)))
            (if experiences
                (begin
                  (set! *experience-buffer* experiences)
                  #t)
                #f))))))

;; Helper: Take first n elements
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;; Functions are exported by default in R5RS

