;;; test-binary-substrate.el --- Tests for Binary Substrate Operations
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Tests for Canonical Binary Substrate (CBS) and binary transformations.

;;; Code:

(require 'meta-log-binary-substrate)
(require 'meta-log-substrate-runtime)

(defun test-binary-create-cbs ()
  "Test CBS creation."
  (meta-log-substrate-initialize)
  (let ((bytes '(1 2 3 4 5))
        (meta '(:encoding "raw")))
    (let ((cbs (meta-log-binary-create-cbs bytes meta)))
      (should (listp cbs))
      (should (eq (car cbs) 'cbs)))))

(defun test-binary-xor ()
  "Test XOR transformation."
  (meta-log-substrate-initialize)
  (let ((bytes '(1 2 3 4))
        (meta '(:encoding "raw"))
        (mask '(255 0 255 0)))
    (let ((cbs (meta-log-binary-create-cbs bytes meta))
          (cbs-id (cadr (meta-log-substrate-create-memory bytes meta))))
      (let ((result (meta-log-binary-xor cbs-id mask)))
        (should (listp result))
        (should (= (length result) 2))))))

(defun test-binary-rotate ()
  "Test bit rotation transformation."
  (meta-log-substrate-initialize)
  (let ((bytes '(1 2 3 4))
        (meta '(:encoding "raw")))
    (let ((cbs-id (cadr (meta-log-substrate-create-memory bytes meta))))
      (let ((result (meta-log-binary-rotate cbs-id 3 'left)))
        (should (listp result))
        (should (= (length result) 2))))))

(defun test-binary-slice ()
  "Test slice extraction."
  (meta-log-substrate-initialize)
  (let ((bytes '(1 2 3 4 5 6 7 8))
        (meta '(:encoding "raw")))
    (let ((cbs-id (cadr (meta-log-substrate-create-memory bytes meta))))
      (let ((result (meta-log-binary-slice cbs-id 2 5)))
        (should (listp result))
        (should (= (length result) 2))))))

(defun test-binary-concat ()
  "Test concatenation of multiple CBS."
  (meta-log-substrate-initialize)
  (let ((bytes1 '(1 2 3))
        (bytes2 '(4 5 6))
        (meta '(:encoding "raw")))
    (let ((id1 (cadr (meta-log-substrate-create-memory bytes1 meta)))
          (id2 (cadr (meta-log-substrate-create-memory bytes2 meta))))
      (let ((result (meta-log-binary-concat id1 id2)))
        (should (listp result))
        (should (= (length result) 2))))))

(ert-deftest binary-substrate-tests ()
  "Run all binary substrate tests."
  (test-binary-create-cbs)
  (test-binary-xor)
  (test-binary-rotate)
  (test-binary-slice)
  (test-binary-concat))

(provide 'test-binary-substrate)

;;; test-binary-substrate.el ends here

