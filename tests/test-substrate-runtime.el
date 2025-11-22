;;; test-substrate-runtime.el --- Tests for Substrate Runtime Protocol
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit and integration tests for substrate runtime operations.

;;; Code:

(require 'meta-log-substrate-runtime)
(require 'meta-log-r5rs)

(defun test-substrate-runtime-initialize ()
  "Test substrate runtime initialization."
  (let ((result (meta-log-substrate-initialize)))
    (should result)
    (should (eq meta-log-substrate--initialized t))))

(defun test-substrate-create-memory ()
  "Test memory object creation."
  (meta-log-substrate-initialize)
  (let ((data '(1 2 3 4))
        (meta '(:encoding "raw" :content-type "binary")))
    (let ((result (meta-log-substrate-create-memory data meta)))
      (should (listp result))
      (should (= (length result) 2))
      (let ((obj (car result))
            (uri (cadr result)))
        (should (listp obj))
        (should (stringp uri))
        (should (string-prefix-p "mlss://" uri))))))

(defun test-substrate-get-memory ()
  "Test memory object retrieval."
  (meta-log-substrate-initialize)
  (let ((data '(1 2 3 4))
        (meta '(:encoding "raw")))
    (let ((created (meta-log-substrate-create-memory data meta))
          (uri (cadr created)))
      (let ((retrieved (meta-log-substrate-get-memory uri)))
        (should retrieved)
        (should (listp retrieved))))))

(defun test-substrate-resolve-uri ()
  "Test mlss:// URI resolution."
  (meta-log-substrate-initialize)
  (let ((data '(5 6 7 8))
        (meta '(:encoding "raw")))
    (let ((created (meta-log-substrate-create-memory data meta))
          (uri (cadr created)))
      (let ((resolved (meta-log-substrate-resolve-uri uri)))
        (should resolved)
        (should (listp resolved))))))

(defun test-substrate-content-addressing ()
  "Test content addressing deduplication."
  (meta-log-substrate-initialize)
  (let ((data '(1 2 3 4))
        (meta '(:encoding "raw")))
    (let ((result1 (meta-log-substrate-create-memory data meta))
          (result2 (meta-log-substrate-create-memory data meta)))
      ;; Same content should produce same URI (content addressing)
      (should (equal (cadr result1) (cadr result2))))))

(ert-deftest substrate-runtime-tests ()
  "Run all substrate runtime tests."
  (test-substrate-runtime-initialize)
  (test-substrate-create-memory)
  (test-substrate-get-memory)
  (test-substrate-resolve-uri)
  (test-substrate-content-addressing))

(provide 'test-substrate-runtime)

;;; test-substrate-runtime.el ends here

