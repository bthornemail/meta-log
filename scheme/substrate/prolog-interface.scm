;;; substrate/prolog-interface.scm --- Prolog/Datalog Interface
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Interface to existing Prolog and Datalog engines via Scheme FFI.
;;; This bridges Scheme code to meta-log-prolog.el and meta-log-datalog.el.

;;; Code:

;; Prolog Interface
;; Uses Geiser FFI to call Emacs Lisp functions via meta-log-prolog-bridge

;; Load shared helpers
(load "../common/helpers.scm")

;; Note: call-emacs-lisp is now defined in common/helpers.scm

(define (prolog-query query)
  "Execute Prolog query.
QUERY: Prolog query as S-expression
Returns list of solutions (binding sets as alists)."
  ;; Call meta-log-bridge-prolog-query via Emacs Lisp bridge
  ;; The marker is intercepted by meta-log-r5rs-eval which executes the function
  (call-emacs-lisp "meta-log-bridge-prolog-query" (list query)))

(define (prolog-add-fact fact)
  "Add fact to Prolog database.
FACT: Prolog fact as S-expression (list with predicate as first element)
Returns success status (#t on success, #f on failure)."
  (let ((predicate (if (and (list? fact) (not (null? fact)))
                       (car fact)
                       fact))
        (args (if (and (list? fact) (> (length fact) 1))
                  (cdr fact)
                  '())))
    ;; Call bridge function - result is intercepted and executed by Emacs side
    (let ((result (call-emacs-lisp "meta-log-bridge-prolog-add-fact"
                                   (cons predicate args))))
      ;; Result should be the fact if successful, or nil on error
      (if result #t #f))))

(define (prolog-add-rule rule)
  "Add rule to Prolog database.
RULE: Prolog rule as S-expression (head . body) or list with :- separator
Returns success status (#t on success, #f on failure)."
  (let ((head (if (and (list? rule) (not (null? rule)))
                  (car rule)
                  rule))
        (body (if (and (list? rule) (> (length rule) 1))
                  (cdr rule)
                  '())))
    ;; Call bridge function - result is intercepted and executed by Emacs side
    (let ((result (call-emacs-lisp "meta-log-bridge-prolog-add-rule"
                                   (cons head body))))
      ;; Result should be the rule if successful, or nil on error
      (if result #t #f))))

;; Datalog Interface

(define (datalog-query query)
  "Execute Datalog query.
QUERY: Datalog query as S-expression
Returns list of matching facts."
  ;; Call meta-log-bridge-datalog-query via Emacs Lisp bridge
  ;; The marker is intercepted by meta-log-r5rs-eval which executes the function
  (call-emacs-lisp "meta-log-bridge-datalog-query" (list query)))

(define (datalog-add-fact fact)
  "Add fact to Datalog database.
FACT: Datalog fact as S-expression (list with predicate as first element)
Returns success status (#t on success, #f on failure)."
  (let ((predicate (if (and (list? fact) (not (null? fact)))
                       (car fact)
                       fact))
        (args (if (and (list? fact) (> (length fact) 1))
                  (cdr fact)
                  '())))
    ;; Call bridge function - result is intercepted and executed by Emacs side
    (let ((result (call-emacs-lisp "meta-log-bridge-datalog-add-fact"
                                   (cons predicate args))))
      ;; Result should be the fact if successful, or nil on error
      (if result #t #f))))

(define (datalog-add-rule rule)
  "Add rule to Datalog database.
RULE: Datalog rule as S-expression (head . body) or list with :- separator
Returns success status (#t on success, #f on failure)."
  (let ((head (if (and (list? rule) (not (null? rule)))
                  (car rule)
                  rule))
        (body (if (and (list? rule) (> (length rule) 1))
                  (cdr rule)
                  '())))
    ;; Call bridge function - result is intercepted and executed by Emacs side
    (let ((result (call-emacs-lisp "meta-log-bridge-datalog-add-rule"
                                   (cons head body))))
      ;; Result should be the rule if successful, or nil on error
      (if result #t #f))))

;; Symbolic Reasoning Integration

(define (symbolic-query engine query)
  "Query symbolic reasoning engine.
ENGINE: 'prolog or 'datalog
QUERY: query as S-expression"
  (case engine
    ((prolog) (prolog-query query))
    ((datalog) (datalog-query query))
    (else (error "Unknown engine" engine))))

(define (symbolic-add engine fact-or-rule)
  "Add fact or rule to symbolic engine.
ENGINE: 'prolog or 'datalog
FACT-OR-RULE: fact or rule as S-expression"
  (case engine
    ((prolog) (if (is-rule? fact-or-rule)
                  (prolog-add-rule fact-or-rule)
                  (prolog-add-fact fact-or-rule)))
    ((datalog) (if (is-rule? fact-or-rule)
                   (datalog-add-rule fact-or-rule)
                   (datalog-add-fact fact-or-rule)))
    (else (error "Unknown engine" engine))))

(define (is-rule? expr)
  "Check if expression is a rule (contains :-)."
  (and (list? expr)
       (>= (length expr) 3)
       (equal? (list-ref expr 1) ':-)))

;; Functions are exported by default in R5RS

