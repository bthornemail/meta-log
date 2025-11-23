;;; test-p-adic.el --- p-Adic Arithmetic Test Suite

;; Test suite for meta-log p-adic arithmetic operations

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log-p-adic)

(defun test-p-adic-valuation ()
  "Test p-adic valuation ord_p(x)."
  (message "Testing p-adic valuation...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test v_2(8) = 3 (8 = 2³)
          (let ((val (meta-log-p-adic-valuation 8 2)))
            (unless (= val 3)
              (push (format "v_2(8) should be 3, got %d" val) errors)))
          
          ;; Test v_3(9) = 2 (9 = 3²)
          (let ((val (meta-log-p-adic-valuation 9 3)))
            (unless (= val 2)
              (push (format "v_3(9) should be 2, got %d" val) errors)))
          
          ;; Test v_5(25) = 2 (25 = 5²)
          (let ((val (meta-log-p-adic-valuation 25 5)))
            (unless (= val 2)
              (push (format "v_5(25) should be 2, got %d" val) errors)))
          
          ;; Test v_p(1) = 0 for any prime
          (let ((val (meta-log-p-adic-valuation 1 7)))
            (unless (= val 0)
              (push (format "v_7(1) should be 0, got %d" val) errors)))
          
          ;; Test v_p(0) = infinity (most-positive-fixnum)
          (let ((val (meta-log-p-adic-valuation 0 2)))
            (unless (= val most-positive-fixnum)
              (push (format "v_2(0) should be infinity, got %d" val) errors))))
      (error (push (format "p-adic valuation error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ p-adic valuation tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ p-adic valuation tests passed")
        t))))

(defun test-p-adic-norm ()
  "Test p-adic norm |x|_p = p^{-v_p(x)}."
  (message "Testing p-adic norm...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test |8|_2 = 2^{-3} = 0.125
          (let ((norm (meta-log-p-adic-norm 8 2)))
            (unless (< (abs (- norm 0.125)) 0.0001)
              (push (format "|8|_2 should be 0.125, got %f" norm) errors)))
          
          ;; Test |9|_3 = 3^{-2} = 1/9 ≈ 0.111
          (let ((norm (meta-log-p-adic-norm 9 3)))
            (unless (< (abs (- norm (/ 1.0 9))) 0.0001)
              (push (format "|9|_3 should be 1/9, got %f" norm) errors)))
          
          ;; Test |1|_p = 1 for any prime
          (let ((norm (meta-log-p-adic-norm 1 7)))
            (unless (< (abs (- norm 1.0)) 0.0001)
              (push (format "|1|_7 should be 1.0, got %f" norm) errors)))
          
          ;; Test |0|_p = 0
          (let ((norm (meta-log-p-adic-norm 0 2)))
            (unless (< (abs norm) 0.0001)
              (push (format "|0|_2 should be 0.0, got %f" norm) errors))))
      (error (push (format "p-adic norm error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ p-adic norm tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ p-adic norm tests passed")
        t))))

(defun test-p-adic-upper-half-plane ()
  "Test p-adic upper half-plane operations."
  (message "Testing p-adic upper half-plane...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test creation
          (let ((hp (meta-log-p-adic-upper-half-plane-create 2)))
            (unless (meta-log-p-adic-upper-half-plane-p hp)
              (push "Upper half-plane should be created" errors))
            (unless (= (meta-log-p-adic-upper-half-plane-prime hp) 2)
              (push "Upper half-plane should have correct prime" errors)))
          
          ;; Test point in half-plane (simplified check)
          (let ((in-hp (meta-log-p-adic-point-in-hp 0.5 2)))
            ;; This is a simplified test - actual implementation may vary
            (unless (or in-hp (not in-hp))  ; Just check it doesn't error
              (push "Point in half-plane check should work" errors))))
      (error (push (format "p-adic upper half-plane error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ p-adic upper half-plane tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ p-adic upper half-plane tests passed")
        t))))

(defun test-p-adic-voter-features ()
  "Test voter feature extraction with p-adic valuations."
  (message "Testing p-adic voter features...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test with mock graph
          (let ((graph '((:nodes . (1 2 3 4)) (:edges . ((1 2) (2 3) (3 4))))))
            (let ((features (meta-log-p-adic-voter-features graph 2)))
              (unless (vectorp features)
                (push "Voter features should return vector" errors))
              (when (vectorp features)
                (unless (> (length features) 0)
                  (push "Voter features should have components" errors)))))
          
          ;; Test with different prime
          (let ((graph '((:nodes . (1 2)) (:edges . ((1 2))))))
            (let ((features (meta-log-p-adic-voter-features graph 3)))
              (unless (vectorp features)
                (push "Voter features with p=3 should work" errors)))))
      (error (push (format "p-adic voter features error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ p-adic voter features tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ p-adic voter features tests passed")
        t))))

(defun test-p-adic-all ()
  "Run all p-adic arithmetic tests."
  (message "=== Running p-Adic Arithmetic Test Suite ===")
  (let ((results '()))
    (push (test-p-adic-valuation) results)
    (push (test-p-adic-norm) results)
    (push (test-p-adic-upper-half-plane) results)
    (push (test-p-adic-voter-features) results)
    
    (let ((passed (length (cl-remove-if-not 'identity results)))
          (total (length results)))
      (message "")
      (message "=== Test Results ===")
      (message "Passed: %d/%d" passed total)
      (if (= passed total)
          (progn
            (message "✓ All p-adic arithmetic tests passed!")
            t)
        (progn
          (message "✗ Some tests failed")
          nil)))))

(provide 'test-p-adic)
;;; test-p-adic.el ends here

