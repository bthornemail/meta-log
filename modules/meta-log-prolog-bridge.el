;;; meta-log-prolog-bridge.el --- Bridge for Scheme to call Prolog/Datalog
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Bridge functions to allow Scheme code to call Prolog/Datalog functions
;;; via Geiser. These functions are registered and can be called from Scheme.

;;; Code:

(require 'meta-log-prolog)
(require 'meta-log-datalog)
(require 'meta-log-http-client nil t)

(defvar meta-log-bridge--prolog-enabled nil
  "Whether Prolog bridge is enabled.")

(defvar meta-log-bridge--datalog-enabled nil
  "Whether Datalog bridge is enabled.")

;; Internal function - auto-enabled by bridge functions
(defun meta-log-bridge--enable ()
  "Enable Prolog/Datalog bridge for Scheme calls (internal)."
  (setq meta-log-bridge--prolog-enabled t)
  (setq meta-log-bridge--datalog-enabled t)
  (message "Prolog/Datalog bridge enabled"))

;; Bridge functions that can be called from Scheme via Geiser

(defun meta-log-bridge-prolog-query (query)
  "Bridge function for Scheme to call meta-log-prolog-query.
QUERY: Prolog query as list (may be nested Scheme structure)
Returns list of binding sets (alists)."
  (meta-log-bridge--enable)  ; Auto-enable if not already
  ;; Convert Scheme structure to Emacs Lisp if needed
  (let ((converted-query (meta-log-bridge--convert-scheme-to-elisp query)))
    (meta-log-prolog-query converted-query)))

(defun meta-log-bridge-prolog-add-fact (predicate &rest args)
  "Bridge function for Scheme to call meta-log-prolog-add-fact.
PREDICATE: predicate symbol (may be string from Scheme)
ARGS: fact arguments
Returns the fact."
  (meta-log-bridge--enable)  ; Auto-enable if not already
  ;; Convert predicate if it's a string
  (let ((pred (if (stringp predicate)
                  (intern predicate)
                predicate))
        (converted-args (mapcar 'meta-log-bridge--convert-scheme-to-elisp args)))
    (apply 'meta-log-prolog-add-fact pred converted-args)))

(defun meta-log-bridge-prolog-add-rule (head &rest body)
  "Bridge function for Scheme to call meta-log-prolog-add-rule.
HEAD: rule head
BODY: rule body goals
Returns the rule."
  (meta-log-bridge--enable)  ; Auto-enable if not already
  (let ((converted-head (meta-log-bridge--convert-scheme-to-elisp head))
        (converted-body (mapcar 'meta-log-bridge--convert-scheme-to-elisp body)))
    (apply 'meta-log-prolog-add-rule converted-head converted-body)))

(defun meta-log-bridge-datalog-query (query)
  "Bridge function for Scheme to call meta-log-datalog-query.
QUERY: Datalog query as list (may be nested Scheme structure)
Returns list of matching facts."
  (meta-log-bridge--enable)  ; Auto-enable if not already
  ;; Convert Scheme structure to Emacs Lisp if needed
  (let ((converted-query (meta-log-bridge--convert-scheme-to-elisp query)))
    (meta-log-datalog-query converted-query)))

(defun meta-log-bridge-datalog-add-fact (predicate &rest args)
  "Bridge function for Scheme to call meta-log-datalog-add-fact.
PREDICATE: predicate symbol (may be string from Scheme)
ARGS: fact arguments
Returns the fact."
  (meta-log-bridge--enable)  ; Auto-enable if not already
  ;; Convert predicate if it's a string
  (let ((pred (if (stringp predicate)
                  (intern predicate)
                predicate))
        (converted-args (mapcar 'meta-log-bridge--convert-scheme-to-elisp args)))
    (apply 'meta-log-datalog-add-fact pred converted-args)))

(defun meta-log-bridge-datalog-add-rule (head &rest body)
  "Bridge function for Scheme to call meta-log-datalog-add-rule.
HEAD: rule head
BODY: rule body goals
Returns the rule."
  (meta-log-bridge--enable)  ; Auto-enable if not already
  (let ((converted-head (meta-log-bridge--convert-scheme-to-elisp head))
        (converted-body (mapcar 'meta-log-bridge--convert-scheme-to-elisp body)))
    (apply 'meta-log-datalog-add-rule converted-head converted-body)))

;; Internal helper function to convert Scheme structures to Emacs Lisp
(defun meta-log-bridge--convert-scheme-to-elisp (obj)
  "Convert Scheme object to Emacs Lisp format.
Handles strings, numbers, symbols, and lists recursively."
  (cond
   ((stringp obj) obj)  ; Strings are the same
   ((numberp obj) obj)  ; Numbers are the same
   ((symbolp obj) obj)  ; Symbols are the same
   ((listp obj)
    ;; Convert list recursively
    (mapcar 'meta-log-bridge--convert-scheme-to-elisp obj))
   (t obj)))  ; Return as-is for other types

;; HTTP Client Bridge Functions

(defun meta-log-bridge-call-vision-api (endpoint data)
  "Bridge function for Scheme to call vision API.
ENDPOINT: string endpoint path
DATA: alist of request data
Returns response data."
  (if (fboundp 'meta-log-http-call-vision-api)
      (meta-log-http-call-vision-api endpoint data)
    (error "HTTP client not available")))

(provide 'meta-log-prolog-bridge)

;;; meta-log-prolog-bridge.el ends here

