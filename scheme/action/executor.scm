;;; action/executor.scm --- Action Execution Layer
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Implements digital action execution for autonomous behavior.
;;; Executes Q* selected actions and provides feedback for learning.

;;; Code:

;; Load substrate modules
(load "../substrate/runtime.scm")

;; Action Types
(define *action-types* '(file-write file-read file-list file-delete
                          network-http network-websocket network-mqtt
                          data-transform data-project-e8 data-store-cbs))

;; Action Result Representation
(define (make-action-result action-id success? outcome data error-message)
  "Create action result representation.
ACTION-ID: identifier for the action
SUCCESS?: #t if successful, #f otherwise
OUTCOME: outcome data (file contents, HTTP response, etc.)
DATA: additional metadata
ERROR-MESSAGE: error description if failed
Returns action result object."
  (list 'action-result
        action-id
        success?
        outcome
        data
        error-message
        (current-timestamp)))

;; Execute Action
(define (execute-action action-spec)
  "Execute Q* selected action.
ACTION-SPEC: action specification from Q*
Returns (action-result outcome) where outcome can be used for Q* feedback."
  (let* ((action-type (list-ref action-spec 1))
         (operator (list-ref action-spec 2))
         (params (list-ref action-spec 3))
         (action-id (uuid-generate)))
    (case action-type
      ((file-write file-read file-list file-delete)
       (execute-file-action action-type operator params action-id))
      ((network-http network-websocket network-mqtt)
       (execute-network-action action-type operator params action-id))
      ((data-transform data-project-e8 data-store-cbs)
       (execute-data-action action-type operator params action-id))
      (else
       (make-action-result action-id #f #f '() "Unknown action type")))))

;; Action Feedback for Q* Learning
(define (action-feedback action-spec action-result)
  "Update Q* values based on action outcomes.
ACTION-SPEC: original action specification
ACTION-RESULT: result from execute-action
Returns feedback data for Q* value update."
  (let* ((success? (list-ref action-result 2))
         (outcome (list-ref action-result 3))
         (reward (if success? 1.0 -1.0))  ; Simple reward: +1 for success, -1 for failure
         (feedback-data `((action-id . ,(list-ref action-result 1))
                         (success . ,success?)
                         (reward . ,reward)
                         (outcome . ,outcome)
                         (timestamp . ,(current-timestamp)))))
    feedback-data))

;; Store Action History in Substrate
(define (store-action-history action-spec action-result feedback)
  "Store action execution history in substrate for learning.
ACTION-SPEC: action specification
ACTION-RESULT: action result
FEEDBACK: feedback data
Returns (memory-object uri)."
  (let* ((data `((action-spec . ,action-spec)
                (action-result . ,action-result)
                (feedback . ,feedback)))
         (meta `((content-type . "action-history")
                 (source-layer . "autonomy")
                 (timestamp . ,(current-timestamp))))
         (memory (substrate-create-memory data meta))
         (uri (list-ref memory 1)))
    (list memory uri)))

;; Helper: Route to specific action handler
(define (execute-file-action action-type operator params action-id)
  "Execute file operation action."
  (load "file-ops.scm")
  (case action-type
    ((file-write) (file-write-action operator params action-id))
    ((file-read) (file-read-action operator params action-id))
    ((file-list) (file-list-action operator params action-id))
    ((file-delete) (file-delete-action operator params action-id))
    (else (make-action-result action-id #f #f '() "Unknown file action type"))))

(define (execute-network-action action-type operator params action-id)
  "Execute network operation action."
  (load "network-ops.scm")
  (case action-type
    ((network-http) (http-request-action operator params action-id))
    ((network-websocket) (websocket-connect-action operator params action-id))
    ((network-mqtt) (mqtt-publish-action operator params action-id))
    (else (make-action-result action-id #f #f '() "Unknown network action type"))))

(define (execute-data-action action-type operator params action-id)
  "Execute data transformation action."
  (load "data-ops.scm")
  (case action-type
    ((data-transform) (transform-data-action operator params action-id))
    ((data-project-e8) (project-e8-action operator params action-id))
    ((data-store-cbs) (store-cbs-action operator params action-id))
    (else (make-action-result action-id #f #f '() "Unknown data action type"))))

;; Functions are exported by default in R5RS

