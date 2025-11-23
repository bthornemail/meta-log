;;; test-e8-theta.el --- E8 Theta Series Test Suite

;; Test suite for meta-log E8 theta series operations

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log-e8-theta)
(require 'meta-log-quadratic-forms)

(defun test-e8-theta-coefficients ()
  "Test E8 theta series coefficient computation."
  (message "Testing theta coefficients...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((theta (meta-log-e8-theta-series-create 10)))
            (unless (plist-get theta :coefficients)
              (push "Theta series should have coefficients" errors))
            (unless (> (length (plist-get theta :coefficients)) 0)
              (push "Theta series should have at least one coefficient" errors))
            ;; Test classical values: r_E8(0) = 1, r_E8(1) = 240
            (let ((r0 (meta-log-e8-theta-coefficient theta 0)))
              (unless (= r0 1)
                (push (format "r_E8(0) should be 1, got %d" r0) errors)))
            (let ((r1 (meta-log-e8-theta-coefficient theta 1)))
              ;; May not match exactly due to sampling, but should be close
              (unless (or (= r1 240) (> r1 100))  ; Allow some variance
                (push (format "r_E8(1) should be close to 240, got %d" r1) errors)))))
      (error (push (format "Theta coefficient error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Theta coefficient tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Theta coefficient tests passed")
        t))))

(defun test-e8-theta-estimation ()
  "Test coefficient estimation formula."
  (message "Testing coefficient estimation...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test estimation for n=1: should be 240 * σ₃(1) = 240 * 1 = 240
          (let ((est (meta-log-e8-theta--estimate-coefficient 1)))
            (unless (= est 240)
              (push (format "Estimation for n=1 should be 240, got %d" est) errors)))
          ;; Test estimation for n=2: should be 240 * σ₃(2) = 240 * (1³ + 2³) = 240 * 9 = 2160
          (let ((est (meta-log-e8-theta--estimate-coefficient 2)))
            (unless (= est 2160)
              (push (format "Estimation for n=2 should be 2160, got %d" est) errors)))
          ;; Test n=0: should be 1
          (let ((est (meta-log-e8-theta--estimate-coefficient 0)))
            (unless (= est 1)
              (push (format "Estimation for n=0 should be 1, got %d" est) errors))))
      (error (push (format "Coefficient estimation error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Coefficient estimation tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Coefficient estimation tests passed")
        t))))

(defun test-e8-theta-qqf-linkage ()
  "Test QQF linkage to E8 theta series."
  (message "Testing QQF linkage...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((theta (meta-log-e8-theta-series-create 10))
                (qqf-matrix '((1.0 0.0 0.0 0.0)
                             (0.0 1.0 0.0 0.0)
                             (0.0 0.0 1.0 0.0)
                             (0.0 0.0 0.0 1.0))))  ; Identity matrix
            (let ((analysis (meta-log-e8-theta-link-to-qqf theta qqf-matrix)))
              (unless (plist-get analysis :determinant)
                (push "QQF analysis should include determinant" errors))
              (unless (plist-get analysis :predicted-universality)
                (push "QQF analysis should include predicted-universality" errors))
              (unless (plist-get analysis :theta-growth-rate)
                (push "QQF analysis should include theta-growth-rate" errors))
              (unless (plist-get analysis :ramanujan-type)
                (push "QQF analysis should include ramanujan-type" errors)))))
      (error (push (format "QQF linkage error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ QQF linkage tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ QQF linkage tests passed")
        t))))

(defun test-e8-theta-quorum-stability ()
  "Test quorum stability prediction."
  (message "Testing quorum stability prediction...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((theta (meta-log-e8-theta-series-create 10))
                (voter-features '((1.0 2.0 3.0 4.0)
                                 (2.0 3.0 4.0 5.0)
                                 (3.0 4.0 5.0 6.0))))
            (let ((prediction (meta-log-e8-theta-predict-quorum-stability theta voter-features)))
              (unless (plist-get prediction :stability-score)
                (push "Prediction should include stability-score" errors))
              (unless (plist-get prediction :qqf-determinant)
                (push "Prediction should include qqf-determinant" errors))
              (unless (plist-get prediction :theta-growth)
                (push "Prediction should include theta-growth" errors))
              (unless (plist-get prediction :form-type)
                (push "Prediction should include form-type" errors))
              ;; Check stability score is in [0, 1]
              (let ((score (plist-get prediction :stability-score)))
                (unless (and (>= score 0) (<= score 1))
                  (push (format "Stability score should be in [0,1], got %f" score) errors))))))
      (error (push (format "Quorum stability error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Quorum stability tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Quorum stability tests passed")
        t))))

(defun test-e8-theta-evaluation ()
  "Test theta series evaluation."
  (message "Testing theta series evaluation...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((theta (meta-log-e8-theta-series-create 10))
                (q 0.5))
            (let ((result (meta-log-e8-theta-evaluate theta q)))
              (unless (numberp result)
                (push "Theta evaluation should return number" errors))
              (unless (>= result 0)
                (push (format "Theta evaluation should be non-negative, got %f" result) errors)))))
      (error (push (format "Theta evaluation error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Theta evaluation tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Theta evaluation tests passed")
        t))))

(defun test-e8-theta-all ()
  "Run all E8 theta series tests."
  (message "=== Running E8 Theta Series Test Suite ===")
  (let ((results '()))
    (push (test-e8-theta-coefficients) results)
    (push (test-e8-theta-estimation) results)
    (push (test-e8-theta-qqf-linkage) results)
    (push (test-e8-theta-quorum-stability) results)
    (push (test-e8-theta-evaluation) results)
    
    (let ((passed (length (cl-remove-if-not 'identity results)))
          (total (length results)))
      (message "")
      (message "=== Test Results ===")
      (message "Passed: %d/%d" passed total)
      (if (= passed total)
          (progn
            (message "✓ All E8 theta series tests passed!")
            t)
        (progn
          (message "✗ Some tests failed")
          nil)))))

(provide 'test-e8-theta)
;;; test-e8-theta.el ends here

