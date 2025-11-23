;;; test-e8-integration.el --- End-to-end E8 integration tests

;; Copyright (C) 2025 Automaton System
;; Author: Automaton System

;; This file is part of meta-log.

;;; Commentary:

;; End-to-end integration tests for E8 modules across Emacs Lisp,
;; TypeScript (via bridge), and Python API service.

;;; Code:

(require 'meta-log-e8)
(require 'meta-log-e8-theta)
(require 'ert)

(defun test-e8-bip32-workflow ()
  "Test complete BIP32 to E8 workflow."
  (let ((lattice (meta-log-e8-lattice-create)))
    ;; Master key
    (let ((master (meta-log-e8-bip32-to-e8 lattice "m/44'/0'/0'")))
      (should (meta-log-e8-point-p master))
      (should (= (length (meta-log-e8-point-coords master)) 8))
      (should (string= (meta-log-e8-point-bip32-path master) "m/44'/0'/0'"))
      
      ;; Child derivation
      (let ((child (meta-log-e8-bip32-to-e8 lattice "m/44'/0'/0'/0/0")))
        (should (meta-log-e8-point-p child))
        
        ;; Distance computation
        (let ((dists (meta-log-e8-distance-for-ml lattice master child)))
          (should (plist-get dists :euclidean))
          (should (>= (plist-get dists :euclidean) 0))
          
          ;; FRBAC verification
          (let ((is-valid (meta-log-e8-verify-frbac-delegation lattice master child)))
            (should (booleanp is-valid))))))))

(defun test-e8-theta-workflow ()
  "Test complete E8 theta series workflow."
  (let ((theta (meta-log-e8-theta-series-create 10)))
    ;; Coefficient lookup
    (let ((coeff-0 (meta-log-e8-theta-coefficient theta 0)))
      (should (>= coeff-0 1)))
    
    (let ((coeff-1 (meta-log-e8-theta-coefficient theta 1)))
      (should (>= coeff-1 240)))
    
    ;; QQF linkage
    (let ((qqf-matrix (make-vector 4 nil)))
      (aset qqf-matrix 0 (vector 1.0 0.0 0.0 0.0))
      (aset qqf-matrix 1 (vector 0.0 1.0 0.0 0.0))
      (aset qqf-matrix 2 (vector 0.0 0.0 1.0 0.0))
      (aset qqf-matrix 3 (vector 0.0 0.0 0.0 1.0))
      (let ((analysis (meta-log-e8-theta-link-to-qqf theta qqf-matrix)))
        (should (plist-get analysis :determinant))
        (should (plist-get analysis :predicted-universality))))))

(defun test-e8-org-babel-integration ()
  "Test E8 Org Babel integration."
  (require 'meta-log-babel)
  ;; Test that Babel functions exist
  (should (fboundp 'org-babel-execute:meta-log-e8))
  (should (fboundp 'org-babel-execute:meta-log-e8-theta)))

(defun test-e8-crypto-integration ()
  "Test E8 integration with crypto module."
  (require 'meta-log-crypto)
  (let ((path "m/44'/0'/0'/0/0"))
    (let ((e8-point (meta-log-crypto-bip32-to-e8 path)))
      (should (meta-log-e8-point-p e8-point))
      (should (string= (meta-log-e8-point-bip32-path e8-point) path)))))

(defun test-e8-partition-integration ()
  "Test E8 integration with partition module."
  (require 'meta-log-partition)
  (let ((lattice (meta-log-e8-lattice-create))
        (point (meta-log-e8-bip32-to-e8 (meta-log-e8-lattice-create) "m/44'/0'/0'")))
    (let ((height (meta-log-partition-padic-height-e8 point 2)))
      (should (numberp height))
      (should (>= height 0)))
    
    (let ((is-ramified (meta-log-partition-detect-e8-ramification point 2 1.0)))
      (should (booleanp is-ramified)))))

(defun test-e8-quadratic-forms-integration ()
  "Test E8 integration with quadratic forms module."
  (require 'meta-log-quadratic-forms)
  (let ((qqf (meta-log-qqf-create (vector (vector 1.0 0.0 0.0 0.0)
                                           (vector 0.0 1.0 0.0 0.0)
                                           (vector 0.0 0.0 1.0 0.0)
                                           (vector 0.0 0.0 0.0 1.0)))))
    (let ((analysis (meta-log-qqf-e8-theta-link qqf)))
      (should (plist-get analysis :determinant))
      (should (plist-get analysis :theta-growth-rate)))))

(ert-deftest e8-integration-bip32-workflow ()
  "Test complete BIP32 to E8 workflow."
  (test-e8-bip32-workflow))

(ert-deftest e8-integration-theta-workflow ()
  "Test complete E8 theta series workflow."
  (test-e8-theta-workflow))

(ert-deftest e8-integration-org-babel ()
  "Test E8 Org Babel integration."
  (test-e8-org-babel-integration))

(ert-deftest e8-integration-crypto ()
  "Test E8 integration with crypto module."
  (test-e8-crypto-integration))

(ert-deftest e8-integration-partition ()
  "Test E8 integration with partition module."
  (test-e8-partition-integration))

(ert-deftest e8-integration-quadratic-forms ()
  "Test E8 integration with quadratic forms module."
  (test-e8-quadratic-forms-integration))

(provide 'test-e8-integration)

;;; test-e8-integration.el ends here

