;;; meta-log-prolog.el --- Prolog engine implementation

;; Copyright (C) 2025 Automaton System
;; Author: Automaton System
;; Version: 1.0.0

;; This file is part of meta-log.

;;; Commentary:

;; Prolog engine with unification and SLD resolution.
;; Ported from R5RS Scheme implementation.

;;; Code:

(require 'cl-lib)

(defvar meta-log-prolog--db nil
  "Prolog database: list of clauses (facts and rules).")

(defun meta-log-prolog-create-db ()
  "Create a new Prolog database."
  (make-hash-table :test 'equal))

(defun meta-log-prolog-variable-p (term)
  "Check if TERM is a Prolog variable.
Variables start with ? or are uppercase symbols."
  (let ((case-fold-search nil))  ; Case-sensitive matching
    (and (or (and (stringp term)
                  (numberp (string-match-p "^?[A-Za-z]" term)))
             (and (symbolp term)
                  (let ((name (symbol-name term)))
                    (or (numberp (string-match-p "^?" name))
                        (numberp (string-match-p "^[A-Z]" name))))))
         t)))

(defun meta-log-prolog-unify (x y bindings)
  "Unify two terms X and Y with existing BINDINGS.
Returns new bindings or :failed if unification fails."
  (cond
   ((equal x y) bindings)
   ((meta-log-prolog-variable-p x)
    (meta-log-prolog-bind x y bindings))
   ((meta-log-prolog-variable-p y)
    (meta-log-prolog-bind y x bindings))
   ((and (listp x) (listp y))
    (let ((b1 (meta-log-prolog-unify (car x) (car y) bindings)))
      (if (eq b1 :failed)
          :failed
        (meta-log-prolog-unify (cdr x) (cdr y) b1))))
   (t :failed)))

(defun meta-log-prolog-bind (var val bindings)
  "Bind variable VAR to value VAL in BINDINGS.
Returns new bindings or :failed if binding fails."
  (let ((existing (assoc var bindings)))
    (if existing
        (meta-log-prolog-unify (cdr existing) val bindings)
      (cons (cons var val) bindings))))

(defun meta-log-prolog-subst (bindings term)
  "Substitute variables in TERM with values from BINDINGS."
  (cond
   ((null term) term)  ; Handle nil/empty list explicitly
   ((meta-log-prolog-variable-p term)
    (let ((binding (assoc term bindings)))
      (if binding
          (meta-log-prolog-subst bindings (cdr binding))
        term)))
   ((listp term)
    (cons (meta-log-prolog-subst bindings (car term))
          (meta-log-prolog-subst bindings (cdr term))))
   (t term)))

(defun meta-log-prolog-add-fact (predicate &rest args)
  "Add a fact to the Prolog database.
PREDICATE is the predicate name, ARGS are the arguments."
  (let ((fact (cons predicate args)))
    ;; Store fact directly as a clause with empty body
    (setq meta-log-prolog--db (cons fact meta-log-prolog--db))
    fact))

(defun meta-log-prolog-add-rule (head &rest body)
  "Add a rule to the Prolog database.
HEAD is the rule head (predicate with args).
BODY is a list of goals (conditions)."
  (let ((rule (cons head body)))
    (setq meta-log-prolog--db (cons rule meta-log-prolog--db))
    rule))

(defun meta-log-prolog-query (goal)
  "Execute a Prolog query.
GOAL is a list (predicate arg1 arg2 ...).
Returns list of binding sets (alists)."
  (unless meta-log-prolog--db
    (setq meta-log-prolog--db '()))
  (meta-log-prolog-resolve goal meta-log-prolog--db '() '()))

(defun meta-log-prolog-resolve (goal clauses bindings proof)
  "Resolve a GOAL against CLAUSES with BINDINGS and PROOF.
Returns list of solutions (binding sets)."
  (if (null clauses)
      '()  ; No more clauses to try
    (let* ((clause (car clauses))
           ;; Handle both facts (just a list) and rules (head . body)
           (head (if (and (listp clause) (symbolp (car clause)))
                    clause  ; It's a fact
                  (car clause)))  ; It's a rule, get the head
           (body (if (and (listp clause) (symbolp (car clause)))
                    '()  ; Facts have no body
                  (cdr clause)))  ; Rules have body
           (unified (meta-log-prolog-unify goal head bindings)))
      (if (not (eq unified :failed))
          (if (null body)
              ;; Fact matched - return this solution and continue searching
              (cons unified (meta-log-prolog-resolve goal (cdr clauses) bindings proof))
            ;; Rule matched - resolve body goals
            (append
             (meta-log-prolog-resolve-body body meta-log-prolog--db unified proof)
             (meta-log-prolog-resolve goal (cdr clauses) bindings proof)))
        ;; No match - try next clause
        (meta-log-prolog-resolve goal (cdr clauses) bindings proof)))))

(defun meta-log-prolog-resolve-body (goals db bindings proof)
  "Resolve a list of GOALS against DB with BINDINGS.
Returns list of binding sets that satisfy all goals."
  (if (null goals)
      (list bindings)  ; All goals satisfied
    (let ((results '()))
      (dolist (solution (meta-log-prolog-resolve (car goals) db bindings proof))
        (setq results (append results
                             (meta-log-prolog-resolve-body (cdr goals) db solution proof))))
      results)))

(defun meta-log-prolog-query-interactive (query-string)
  "Execute a Prolog query from a string.
QUERY-STRING is a Prolog query like \"node(?Id, ?Type)\"."
  (interactive "sProlog query: ")
  (let ((goal (meta-log-prolog-parse-query query-string)))
    (let ((results (meta-log-prolog-query goal)))
      (if results
          (progn
            (message "Found %d solution(s)" (length results))
            (with-output-to-temp-buffer "*meta-log-prolog-results*"
              (dolist (result results)
                (princ (format "%S\n" result)))))
        (message "No solutions found")))))

(defun meta-log-prolog-parse-query (query-string)
  "Parse a Prolog query string into a goal list.
QUERY-STRING format: \"predicate(arg1, arg2)\""
  ;; Simple parser - can be enhanced later
  (condition-case err
      (read query-string)
    (error
     (user-error "Invalid Prolog query: %s" query-string))))

(provide 'meta-log-prolog)

;;; meta-log-prolog.el ends here

