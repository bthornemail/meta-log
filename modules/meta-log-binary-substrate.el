;;; meta-log-binary-substrate.el --- Canonical Binary Substrate Interface
;;; Copyright (C) 2025 Meta-Log Research Group
;;; Author: Meta-Log Research Group

;;; Commentary:
;;; Emacs Lisp interface to Binary Layer Protocol (BLP) with Canonical
;;; Binary Substrate (CBS) format implemented in R5RS Scheme.

;;; Code:

(require 'meta-log-substrate-runtime)

;;;###autoload
(defun meta-log-binary-create-cbs (bytes meta)
  "Create Canonical Binary Substrate (CBS) from BYTES and META.
BYTES: bytevector or list of bytes
META: plist of metadata
Returns CBS object."
  (interactive)
  (meta-log-substrate-create-memory bytes
                                    (plist-put meta :encoding "raw")))

;;;###autoload
(defun meta-log-binary-transform (input-id operator params)
  "Transform binary substrate.
INPUT-ID: URI or ID of input CBS
OPERATOR: transformation operator ('xor 'rotate 'slice 'concat)
PARAMS: plist of parameters
Returns (result-cbs uri)."
  (interactive)
  (let ((operator-str (symbol-name operator))
        (params-expr (format "'%S" params))
        (call-expr (format "(substrate-transform %S %S %s)"
                          input-id operator-str params-expr)))
    (meta-log-r5rs-eval call-expr)))

;;;###autoload
(defun meta-log-binary-xor (cbs-id mask)
  "XOR transform on CBS with MASK.
CBS-ID: URI or ID of CBS
MASK: bytevector or list of bytes
Returns (result-cbs uri)."
  (interactive)
  (meta-log-binary-transform cbs-id
                            'xor
                            `(:mask ,mask)))

;;;###autoload
(defun meta-log-binary-rotate (cbs-id n-bits direction)
  "Rotate bits in CBS.
CBS-ID: URI or ID of CBS
N-BITS: number of bits to rotate
DIRECTION: 'left or 'right
Returns (result-cbs uri)."
  (interactive)
  (meta-log-binary-transform cbs-id
                            'rotate
                            `(:n-bits ,n-bits :direction ,direction)))

;;;###autoload
(defun meta-log-binary-slice (cbs-id start end)
  "Extract slice from CBS.
CBS-ID: URI or ID of CBS
START: start index
END: end index
Returns (result-cbs uri)."
  (interactive)
  (meta-log-binary-transform cbs-id
                            'slice
                            `(:start ,start :end ,end)))

;;;###autoload
(defun meta-log-binary-concat (&rest cbs-ids)
  "Concatenate multiple CBS objects.
CBS-IDS: list of CBS URIs or IDs
Returns (result-cbs uri)."
  (interactive)
  (meta-log-binary-transform (car cbs-ids)
                            'concat
                            `(:inputs ,cbs-ids)))

(provide 'meta-log-binary-substrate)

;;; meta-log-binary-substrate.el ends here

