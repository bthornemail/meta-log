;;; meta-log-substrate-runtime.el --- Substrate Runtime Protocol Interface
;;; Copyright (C) 2025 Meta-Log Research Group
;;; Author: Meta-Log Research Group

;;; Commentary:
;;; Emacs Lisp interface to Substrate Runtime Protocol (SRP) implemented
;;; in R5RS Scheme. Provides memory model, content addressing, and
;;; deterministic scheduling.

;;; Code:

(require 'meta-log-r5rs)

(defvar meta-log-substrate--initialized nil
  "Whether substrate runtime has been initialized.")

;;;###autoload
(defun meta-log-substrate-initialize ()
  "Initialize substrate runtime.
Loads R5RS substrate engine."
  (interactive)
  (unless meta-log-substrate--initialized
    (let ((engine-path (expand-file-name "scheme/r5rs-canvas-engine.scm"
                                          (file-name-directory (locate-library "meta-log")))))
      (when (file-exists-p engine-path)
        (meta-log-r5rs-load-engine engine-path)
        (setq meta-log-substrate--initialized t)
        (message "Substrate runtime initialized"))
      (unless meta-log-substrate--initialized
        (message "Warning: Substrate engine not found at %s" engine-path))))
  meta-log-substrate--initialized)

;;;###autoload
(defun meta-log-substrate-create-memory (data meta)
  "Create memory object from DATA and META.
DATA: bytevector or list of bytes
META: plist of metadata
Returns (memory-object uri)."
  (interactive)
  (meta-log-substrate-initialize)
  (let ((data-expr (if (listp data)
                       (format "#u8(%s)" (mapconcat #'number-to-string data " "))
                     (format "#u8(%s)" data)))
        (meta-expr (format "'%S" meta))
        (call-expr (format "(substrate-create-memory %s %s)" data-expr meta-expr)))
    (meta-log-r5rs-eval call-expr)))

;;;###autoload
(defun meta-log-substrate-get-memory (uri-or-id)
  "Get memory object by URI or ID.
URI-OR-ID: mlss:// URI or memory ID
Returns memory object."
  (interactive "sURI or ID: ")
  (meta-log-substrate-initialize)
  (let ((call-expr (format "(substrate-get-memory %S)" uri-or-id)))
    (meta-log-r5rs-eval call-expr)))

;;;###autoload
(defun meta-log-substrate-resolve-uri (uri)
  "Resolve mlss:// URI to memory object.
URI: mlss:// content address
Returns memory object."
  (interactive "smlss:// URI: ")
  (meta-log-substrate-initialize)
  (let ((call-expr (format "(substrate-resolve-uri %S)" uri)))
    (meta-log-r5rs-eval call-expr)))

(provide 'meta-log-substrate-runtime)

;;; meta-log-substrate-runtime.el ends here

