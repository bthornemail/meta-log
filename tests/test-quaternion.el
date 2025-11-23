;;; test-quaternion.el --- Quaternion Algebra Test Suite

;; Test suite for meta-log quaternion algebra operations

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log-quaternion)
(require 'meta-log-p-adic)

(defun test-quaternion-algebra-creation ()
  "Test quaternion algebra creation."
  (message "Testing quaternion algebra creation...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test Hamilton's quaternions (-1, -1/ℚ)
          (let ((alg (meta-log-quaternion-algebra-create -1 -1)))
            (unless (meta-log-quaternion-algebra-p alg)
              (push "Quaternion algebra should be created" errors))
            (unless (= (meta-log-quaternion-algebra-a alg) -1)
              (push "Quaternion algebra should have correct a" errors))
            (unless (= (meta-log-quaternion-algebra-b alg) -1)
              (push "Quaternion algebra should have correct b" errors)))
          
          ;; Test split quaternion algebra (1, 1/ℚ)
          (let ((alg (meta-log-quaternion-algebra-create 1 1)))
            (unless (meta-log-quaternion-algebra-p alg)
              (push "Split quaternion algebra should be created" errors))))
      (error (push (format "Quaternion algebra creation error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Quaternion algebra creation tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Quaternion algebra creation tests passed")
        t))))

(defun test-quaternion-norm-form ()
  "Test quaternion norm form calculation."
  (message "Testing quaternion norm form...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test norm for element t + xi + yj + zk = 1 + 0i + 0j + 0k
          (let ((alg (meta-log-quaternion-algebra-create -1 -1))
                (q (meta-log-quaternion-element-create 1 0 0 0 alg)))
            (let ((norm (meta-log-quaternion-norm q)))
              (unless (= norm 1)
                (push (format "Norm of 1 should be 1, got %f" norm) errors))))
          
          ;; Test norm for element 0 + 1i + 0j + 0k
          (let* ((alg (meta-log-quaternion-algebra-create -1 -1))
                 (q (meta-log-quaternion-element-create 0 1 0 0 alg))
                 (norm (meta-log-quaternion-norm q)))
            ;; Norm should be t² - a x² - b y² + a b z² = 0 - (-1)*1 - 0 + 0 = 1
            (unless (= norm 1)
              (push (format "Norm of i should be 1, got %f" norm) errors))))
      (error (push (format "Quaternion norm error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Quaternion norm tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Quaternion norm tests passed")
        t))))

(defun test-quaternion-hilbert-symbol ()
  "Test Hilbert symbol computation."
  (message "Testing Hilbert symbol...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test Hilbert symbol for (-1, -1/ℚ) at p=2
          (let ((alg (meta-log-quaternion-algebra-create -1 -1)))
            (let ((symbol (meta-log-quaternion-hilbert-symbol alg 2)))
              ;; Should return 1 or -1
              (unless (member symbol '(1 -1))
                (push (format "Hilbert symbol should be 1 or -1, got %d" symbol) errors))))
          
          ;; Test Hilbert symbol at odd prime
          (let ((alg (meta-log-quaternion-algebra-create -1 -1)))
            (let ((symbol (meta-log-quaternion-hilbert-symbol alg 3)))
              (unless (member symbol '(1 -1))
                (push (format "Hilbert symbol at p=3 should be 1 or -1, got %d" symbol) errors)))))
      (error (push (format "Hilbert symbol error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Hilbert symbol tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Hilbert symbol tests passed")
        t))))

(defun test-quaternion-discriminant ()
  "Test quaternion discriminant (ramified primes)."
  (message "Testing quaternion discriminant...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test discriminant for (-1, -1/ℚ)
          (let ((alg (meta-log-quaternion-algebra-create -1 -1)))
            (let ((ramified (meta-log-quaternion-discriminant alg)))
              (unless (listp ramified)
                (push "Discriminant should return list of primes" errors))
              ;; Should return some ramified primes
              (when (null ramified)
                (message "Note: No ramified primes found (may be correct)")))))
      (error (push (format "Quaternion discriminant error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Quaternion discriminant tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Quaternion discriminant tests passed")
        t))))

(defun test-quaternion-bip32-path ()
  "Test BIP32 path mapping to quaternion basis."
  (message "Testing BIP32 path mapping...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test simple path
          (let* ((alg (meta-log-quaternion-algebra-create -1 -1))
                 (path "m/44'/0'/0'/0/0")
                 (q (meta-log-quaternion-bip32-path alg path)))
            (unless (meta-log-quaternion-element-p q)
              (push "BIP32 path should create quaternion element" errors))
            (when q
              (unless (eq (meta-log-quaternion-element-algebra q) alg)
                (push "Quaternion element should have correct algebra" errors))))
          
          ;; Test path with more components
          (let* ((alg (meta-log-quaternion-algebra-create -1 -1))
                 (path "m/44'/meta-log'/0'/0/0")
                 (q (meta-log-quaternion-bip32-path alg path)))
            (unless (meta-log-quaternion-element-p q)
              (push "Complex BIP32 path should work" errors))))
      (error (push (format "BIP32 path mapping error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ BIP32 path mapping tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ BIP32 path mapping tests passed")
        t))))

(defun test-quaternion-all ()
  "Run all quaternion algebra tests."
  (message "=== Running Quaternion Algebra Test Suite ===")
  (let ((results '()))
    (push (test-quaternion-algebra-creation) results)
    (push (test-quaternion-norm-form) results)
    (push (test-quaternion-hilbert-symbol) results)
    (push (test-quaternion-discriminant) results)
    (push (test-quaternion-bip32-path) results)
    
    (let ((passed (length (cl-remove-if-not 'identity results)))
          (total (length results)))
      (message "")
      (message "=== Test Results ===")
      (message "Passed: %d/%d" passed total)
      (if (= passed total)
          (progn
            (message "✓ All quaternion algebra tests passed!")
            t)
        (progn
          (message "✗ Some tests failed")
          nil)))))

(provide 'test-quaternion)
;;; test-quaternion.el ends here

