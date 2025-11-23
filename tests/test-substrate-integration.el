;;; test-substrate-integration.el --- End-to-end integration tests
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Integration tests for complete substrate workflows.

;;; Code:

(require 'meta-log-substrate-runtime)
(require 'meta-log-binary-substrate)
(require 'meta-log-provenance)

(defun test-substrate-workflow-binary-transform ()
  "Test complete binary transformation workflow."
  (meta-log-substrate-initialize)
  (let ((data '(1 2 3 4 5 6 7 8))
        (meta '(:encoding "raw")))
    ;; Create memory
    (let ((created (meta-log-substrate-create-memory data meta))
          (uri (cadr created)))
      ;; Transform
      (let ((transformed (meta-log-binary-xor uri '(255 0 255 0))))
        (should transformed)
        ;; Get provenance
        (let ((provenance (meta-log-provenance-get (cadr transformed))))
          (should (listp provenance)))))))

(defun test-substrate-workflow-content-addressing ()
  "Test content addressing workflow."
  (meta-log-substrate-initialize)
  (let ((data '(10 20 30 40))
        (meta '(:encoding "raw")))
    ;; Create same content twice
    (let ((result1 (meta-log-substrate-create-memory data meta))
          (result2 (meta-log-substrate-create-memory data meta)))
      ;; Should get same URI (content addressing)
      (should (equal (cadr result1) (cadr result2)))
      ;; Resolve should return same object
      (let ((resolved1 (meta-log-substrate-resolve-uri (cadr result1)))
            (resolved2 (meta-log-substrate-resolve-uri (cadr result2))))
        (should resolved1)
        (should resolved2)))))

(defun test-substrate-workflow-provenance-chain ()
  "Test provenance chain through multiple transformations."
  (meta-log-substrate-initialize)
  (let ((data '(1 2 3 4))
        (meta '(:encoding "raw")))
    (let ((uri1 (cadr (meta-log-substrate-create-memory data meta))))
      ;; First transformation
      (let ((uri2 (cadr (meta-log-binary-xor uri1 '(255 0 255 0)))))
        ;; Second transformation
        (let ((uri3 (cadr (meta-log-binary-rotate uri2 2 'left))))
          ;; Check provenance chain
          (let ((provenance (meta-log-provenance-get uri3)))
            (should (listp provenance))
            ;; Chain should have at least 2 transformations
            (should (>= (length provenance) 0))))))))

(ert-deftest substrate-integration-tests ()
  "Run all integration tests."
  (test-substrate-workflow-binary-transform)
  (test-substrate-workflow-content-addressing)
  (test-substrate-workflow-provenance-chain))

(provide 'test-substrate-integration)

;;; test-substrate-integration.el ends here

