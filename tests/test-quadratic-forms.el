;;; test-quadratic-forms.el --- Quadratic Forms Test Suite

;; Test suite for meta-log quadratic forms (BQF, TQF, QQF)

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log-quadratic-forms)

(defun test-quadratic-forms-bqf-discriminant ()
  "Test BQF discriminant calculation Δ = b² - 4ac."
  (message "Testing BQF discriminant calculation...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test case: x² + xy + y² (a=1, b=1, c=1)
          (let ((bqf (meta-log-bqf-create 1 1 1))
                (expected-delta -3))
            (let ((delta (meta-log-bqf-discriminant bqf)))
              (unless (= delta expected-delta)
                (push (format "BQF discriminant failed: expected %d, got %d" expected-delta delta) errors))))
          
          ;; Test case: x² - y² (a=1, b=0, c=-1) -> Δ = 4
          (let ((bqf (meta-log-bqf-create 1 0 -1))
                (expected-delta 4))
            (let ((delta (meta-log-bqf-discriminant bqf)))
              (unless (= delta expected-delta)
                (push (format "BQF discriminant (indefinite) failed: expected %d, got %d" expected-delta delta) errors)))))
      (error (push (format "BQF discriminant error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ BQF discriminant tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ BQF discriminant tests passed")
        t))))

(defun test-quadratic-forms-bqf-classification ()
  "Test BQF classification (positive-definite, indefinite, degenerate)."
  (message "Testing BQF classification...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Positive definite: x² + xy + y² (Δ = -3 < 0, a = 1 > 0)
          (let ((bqf (meta-log-bqf-create 1 1 1)))
            (unless (eq (meta-log-bqf-classify bqf) 'positive-definite)
              (push "BQF should be positive-definite" errors)))
          
          ;; Indefinite: x² - y² (Δ = 4 > 0)
          (let ((bqf (meta-log-bqf-create 1 0 -1)))
            (unless (eq (meta-log-bqf-classify bqf) 'indefinite)
              (push "BQF should be indefinite" errors)))
          
          ;; Degenerate: x² + 2xy + y² = (x+y)² (Δ = 0)
          (let ((bqf (meta-log-bqf-create 1 2 1)))
            (unless (eq (meta-log-bqf-classify bqf) 'degenerate)
              (push "BQF should be degenerate" errors))))
      (error (push (format "BQF classification error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ BQF classification tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ BQF classification tests passed")
        t))))

(defun test-quadratic-forms-tqf ()
  "Test TQF discriminant and classification."
  (message "Testing TQF...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test TQF: x² + y² + z² (a=1, b=1, c=1, d=0, e=0, f=0)
          (let ((tqf (meta-log-tqf-create 1 1 1 0 0 0)))
            (let ((delta (meta-log-tqf-discriminant tqf)))
              (unless (> delta 0)
                (push (format "TQF discriminant should be positive, got %d" delta) errors)))
            (unless (eq (meta-log-tqf-classify tqf) 'positive-definite)
              (push "TQF should be positive-definite" errors))))
      (error (push (format "TQF error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ TQF tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ TQF tests passed")
        t))))

(defun test-quadratic-forms-qqf ()
  "Test QQF matrix determinant calculation."
  (message "Testing QQF...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test QQF: x² + y² + z² + w² (identity matrix, det = 1)
          (let ((qqf (meta-log-qqf-create 1 1 1 1 0 0 0 0 0 0)))
            (let ((delta (meta-log-qqf-discriminant qqf)))
              (unless (> delta 0)
                (push (format "QQF discriminant should be positive, got %f" delta) errors)))
            (unless (eq (meta-log-qqf-classify qqf) 'positive-definite)
              (push "QQF should be positive-definite" errors))))
      (error (push (format "QQF error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ QQF tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ QQF tests passed")
        t))))

(defun test-quadratic-forms-canvasl-extraction ()
  "Test BQF extraction from CanvasL data."
  (message "Testing CanvasL BQF extraction...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Simulate CanvasL BQF object (vector format)
          (let ((bqf-obj `((coefficients . ,(vector 1 1 1)))))
            (let ((bqf (meta-log-bqf-from-canvasl bqf-obj)))
              (unless bqf
                (push "BQF extraction from CanvasL should succeed" errors))
              (when bqf
                (unless (= (meta-log-bqf-discriminant bqf) -3)
                  (push "Extracted BQF should have correct discriminant" errors)))))
          
          ;; Test with list format
          (let ((bqf-obj '((coefficients . (1 1 1)))))
            (let ((bqf (meta-log-bqf-from-canvasl bqf-obj)))
              (unless bqf
                (push "BQF extraction from CanvasL list should succeed" errors))))
          
          ;; Test with coefficient list
          (let ((bqf (meta-log-bqf-from-coefficients '(1 1 1))))
            (unless bqf
              (push "BQF from coefficients should succeed" errors))
            (when bqf
              (unless (= (meta-log-bqf-discriminant bqf) -3)
                (push "BQF from coefficients should have correct discriminant" errors)))))
      (error (push (format "CanvasL extraction error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ CanvasL extraction tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ CanvasL extraction tests passed")
        t))))

(defun test-quadratic-forms-consensus-classification ()
  "Test consensus classification integration."
  (message "Testing consensus classification...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test BQF consensus classification
          (let ((bqf (meta-log-bqf-create 1 1 1)))
            (let ((result (meta-log-quadratic-forms-classify-consensus 'bqf bqf)))
              (unless result
                (push "Consensus classification should return result" errors))
              (when result
                (unless (cdr (assq :classification result))
                  (push "Consensus classification should have classification" errors))
                (unless (cdr (assq :stable result))
                  (push "Consensus classification should have stability flag" errors)))))
          
          ;; Test with CanvasL data
          (let ((bqf-obj '((coefficients . [1 1 1]))))
            (let ((result (meta-log-quadratic-forms-classify-consensus 'bqf bqf-obj)))
              (unless result
                (push "Consensus classification with CanvasL should work" errors)))))
      (error (push (format "Consensus classification error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Consensus classification tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Consensus classification tests passed")
        t))))

(defun test-quadratic-forms-all ()
  "Run all quadratic forms tests."
  (message "=== Running Quadratic Forms Test Suite ===")
  (let ((results '()))
    (push (test-quadratic-forms-bqf-discriminant) results)
    (push (test-quadratic-forms-bqf-classification) results)
    (push (test-quadratic-forms-tqf) results)
    (push (test-quadratic-forms-qqf) results)
    (push (test-quadratic-forms-canvasl-extraction) results)
    (push (test-quadratic-forms-consensus-classification) results)
    
    (let ((passed (length (cl-remove-if-not 'identity results)))
          (total (length results)))
      (message "")
      (message "=== Test Results ===")
      (message "Passed: %d/%d" passed total)
      (if (= passed total)
          (progn
            (message "✓ All quadratic forms tests passed!")
            t)
        (progn
          (message "✗ Some tests failed")
          nil)))))

(provide 'test-quadratic-forms)
;;; test-quadratic-forms.el ends here

