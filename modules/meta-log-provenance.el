;;; meta-log-provenance.el --- Provenance Chain Protocol Interface
;;; Copyright (C) 2025 Meta-Log Research Group
;;; Author: Meta-Log Research Group

;;; Commentary:
;;; Emacs Lisp interface to Provenance Chain Protocol (PCP) implemented
;;; in R5RS Scheme. Provides provenance tracking and verification.

;;; Code:

(require 'meta-log-substrate-runtime)

;;;###autoload
(defun meta-log-provenance-get (uri-or-hash)
  "Get provenance information.
URI-OR-HASH: memory URI or provenance hash
Returns list of provenance records."
  (interactive "sURI or hash: ")
  (let ((call-expr (format "(substrate-get-provenance %S)" uri-or-hash)))
    (meta-log-r5rs-eval call-expr)))

;;;###autoload
(defun meta-log-provenance-verify (chain)
  "Verify provenance chain integrity.
CHAIN: list of provenance records
Returns (valid? errors)."
  (interactive)
  (let ((call-expr (format "(substrate-verify-provenance '%S)" chain)))
    (meta-log-r5rs-eval call-expr)))

;;;###autoload
(defun meta-log-provenance-record-transformation (operation input-uris output-uris cost-metrics)
  "Record a transformation in provenance chain.
OPERATION: plist with :type :operator :params
INPUT-URIS: list of input memory URIs
OUTPUT-URIS: list of output memory URIs
COST-METRICS: plist of cost metrics
Returns provenance hash."
  (let ((operation-expr (format "'%S" operation))
        (input-expr (format "'%S" input-uris))
        (output-expr (format "'%S" output-uris))
        (cost-expr (format "'%S" cost-metrics))
        (call-expr (format "(record-transformation %s %s %s %s)"
                          operation-expr input-expr output-expr cost-expr)))
    (meta-log-r5rs-eval call-expr)))

(provide 'meta-log-provenance)

;;; meta-log-provenance.el ends here

