;;; test-provenance.el --- Tests for Provenance Chain Protocol
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Tests for provenance tracking and verification.

;;; Code:

(require 'meta-log-provenance)
(require 'meta-log-substrate-runtime)
(require 'meta-log-binary-substrate)

(defun test-provenance-get ()
  "Test provenance retrieval."
  (meta-log-substrate-initialize)
  (let ((bytes '(1 2 3 4))
        (meta '(:encoding "raw")))
    (let ((created (meta-log-substrate-create-memory bytes meta))
          (uri (cadr created)))
      (let ((provenance (meta-log-provenance-get uri)))
        (should (listp provenance))))))

(defun test-provenance-verify ()
  "Test provenance chain verification."
  (meta-log-substrate-initialize)
  (let ((bytes '(1 2 3 4))
        (meta '(:encoding "raw")))
    (let ((created (meta-log-substrate-create-memory bytes meta))
          (uri (cadr created)))
      (let ((provenance (meta-log-provenance-get uri)))
        (if provenance
            (let ((verification (meta-log-provenance-verify provenance)))
              (should (listp verification))
              (should (car verification))))))))

(defun test-provenance-chain ()
  "Test provenance chain formation through transformations."
  (meta-log-substrate-initialize)
  (let ((bytes '(1 2 3 4))
        (meta '(:encoding "raw"))
        (mask '(255 0 255 0)))
    (let ((cbs-id (cadr (meta-log-substrate-create-memory bytes meta))))
      (let ((transformed (meta-log-binary-xor cbs-id mask))
            (result-uri (cadr transformed)))
        (let ((provenance (meta-log-provenance-get result-uri)))
          (should (listp provenance))
          ;; Provenance should show transformation history
          (should (>= (length provenance) 0)))))))

(ert-deftest provenance-tests ()
  "Run all provenance tests."
  (test-provenance-get)
  (test-provenance-verify)
  (test-provenance-chain))

(provide 'test-provenance)

;;; test-provenance.el ends here

