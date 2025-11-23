;;; consciousness/reflection.scm --- Self-Reflection
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Self-reflection on decisions and actions.
;;; Analyzes decision quality, evaluates outcomes, and updates self-model.

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")
(load "state.scm")
(load "self-monitoring.scm")

;; Reflection History
(define *reflection-history* '())

;; Reflect on Decision
(define (reflect-on-decision decision decision-context outcome)
  "Analyze decision quality.
DECISION: decision that was made
DECISION-CONTEXT: context in which decision was made
OUTCOME: outcome of decision
Returns reflection analysis."
  (let* ((decision-quality (evaluate-decision-quality decision outcome))
         (lessons-learned (extract-lessons decision outcome))
         (reflection `((decision . ,decision)
                      (context . ,decision-context)
                      (outcome . ,outcome)
                      (quality . ,decision-quality)
                      (lessons . ,lessons-learned)
                      (timestamp . ,(current-timestamp)))))
    (set! *reflection-history* (cons reflection *reflection-history*))
    reflection))

;; Reflect on Action
(define (reflect-on-action action action-result)
  "Evaluate action outcomes.
ACTION: action that was executed
ACTION-RESULT: result from action execution
Returns reflection analysis."
  (let* ((success? (list-ref action-result 2))
         (outcome (list-ref action-result 3))
         (action-quality (evaluate-action-quality action success? outcome))
         (improvement-suggestions (suggest-improvements action action-result))
         (reflection `((action . ,action)
                      (result . ,action-result)
                      (quality . ,action-quality)
                      (suggestions . ,improvement-suggestions)
                      (timestamp . ,(current-timestamp)))))
    (set! *reflection-history* (cons reflection *reflection-history*))
    reflection))

;; Evaluate Decision Quality
(define (evaluate-decision-quality decision outcome)
  "Evaluate how good a decision was.
DECISION: decision object
OUTCOME: outcome of decision
Returns quality score (0-1)."
  (let ((outcome-success (if (list? outcome)
                            (assoc-ref outcome 'success)
                            #f)))
    (if outcome-success
        0.8  ; Good decision if outcome was successful
        0.3)))  ; Poor decision if outcome failed

;; Evaluate Action Quality
(define (evaluate-action-quality action success? outcome)
  "Evaluate how good an action was.
ACTION: action object
SUCCESS?: whether action succeeded
OUTCOME: action outcome
Returns quality score (0-1)."
  (if success?
      (let ((outcome-value (if (list? outcome)
                              (let ((val (assoc-ref outcome 'value)))
                                (if (number? val) val 0.5))
                              0.5)))
        (max 0.5 outcome-value))
      0.2))  ; Low quality if action failed

;; Extract Lessons
(define (extract-lessons decision outcome)
  "Extract lessons learned from decision and outcome.
DECISION: decision object
OUTCOME: outcome
Returns list of lesson strings."
  (let ((lessons '()))
    (if (and (list? outcome) (assoc-ref outcome 'success))
        (set! lessons (cons "Decision led to successful outcome" lessons))
        (set! lessons (cons "Decision did not achieve desired outcome" lessons)))
    lessons))

;; Suggest Improvements
(define (suggest-improvements action action-result)
  "Suggest improvements for future actions.
ACTION: action object
ACTION-RESULT: action result
Returns list of improvement suggestions."
  (let ((suggestions '())
        (success? (list-ref action-result 2)))
    (if (not success?)
        (begin
          (set! suggestions (cons "Consider alternative action type" suggestions))
          (set! suggestions (cons "Check action parameters before execution" suggestions))))
    suggestions))

;; Update Self-Model
(define (update-self-model reflection)
  "Update internal model of self based on reflection.
REFLECTION: reflection analysis
Returns updated self-model."
  (let* ((quality (assoc-ref reflection 'quality))
         (lessons (assoc-ref reflection 'lessons))
         (self-model `((average-decision-quality . ,quality)
                      (lessons-learned . ,lessons)
                      (reflection-count . ,(length *reflection-history*))
                      (last-updated . ,(current-timestamp)))))
    self-model))

;; Store Reflection in Substrate
(define (store-reflection reflection)
  "Store reflection in substrate for persistent learning.
REFLECTION: reflection object
Returns (memory-object uri)."
  (let* ((data reflection)
         (meta `((content-type . "reflection")
                (source-layer . "consciousness")
                (timestamp . ,(current-timestamp))))
         (memory (substrate-create-memory data meta))
         (uri (list-ref memory 1)))
    (list memory uri)))

;; Functions are exported by default in R5RS

