;;; test-geometric-alignments.el --- Geometric Alignments Test Suite

;; Test suite for meta-log geometric curve parametrizations

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log-geometric-alignments)
(require 'meta-log-p-adic)

(defun test-geometric-deltoid ()
  "Test deltoid parametrization (3 cusps)."
  (message "Testing deltoid parametrization...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test deltoid at t=0
          (let ((coords (meta-log-deltoid-parametrization 0)))
            (unless (listp coords)
              (push "Deltoid should return coordinates" errors))
            (when (listp coords)
              (unless (= (length coords) 2)
                (push "Deltoid should return (x, y)" errors))))
          
          ;; Test deltoid at t=π
          (let ((coords (meta-log-deltoid-parametrization pi)))
            (unless (listp coords)
              (push "Deltoid at π should work" errors)))
          
          ;; Test 3 cusp points
          (let ((cusps (meta-log-deltoid-3-cusp-points)))
            (unless (listp cusps)
              (push "Deltoid cusps should return list" errors))
            (when (listp cusps)
              (unless (= (length cusps) 3)
                (push "Deltoid should have 3 cusps" errors)))))
      (error (push (format "Deltoid error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Deltoid tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Deltoid tests passed")
        t))))

(defun test-geometric-astroid ()
  "Test astroid parametrization (4 cusps)."
  (message "Testing astroid parametrization...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test astroid at t=0
          (let ((coords (meta-log-astroid-parametrization 0)))
            (unless (listp coords)
              (push "Astroid should return coordinates" errors))
            (when (listp coords)
              (unless (= (length coords) 2)
                (push "Astroid should return (x, y)" errors))))
          
          ;; Test astroid at t=π/2
          (let ((coords (meta-log-astroid-parametrization (/ pi 2))))
            (unless (listp coords)
              (push "Astroid at π/2 should work" errors)))
          
          ;; Test 4 cusp points
          (let ((cusps (meta-log-astroid-4-cusp-points)))
            (unless (listp cusps)
              (push "Astroid cusps should return list" errors))
            (when (listp cusps)
              (unless (= (length cusps) 4)
                (push "Astroid should have 4 cusps" errors))))
          
          ;; Test quaternion symmetry
          (let ((symmetry (meta-log-astroid-quaternion-symmetry)))
            (unless symmetry
              (push "Astroid quaternion symmetry should return result" errors))
            (when symmetry
              (unless (eq (cdr (assq :type symmetry)) 'quaternion-symmetry)
                (push "Astroid should have quaternion symmetry type" errors)))))
      (error (push (format "Astroid error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Astroid tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Astroid tests passed")
        t))))

(defun test-geometric-epicycloid ()
  "Test epicycloid roulette traces."
  (message "Testing epicycloid parametrization...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test epicycloid with R=5, r=2
          (let ((coords (meta-log-epicycloid-parametrization 0 5 2)))
            (unless (listp coords)
              (push "Epicycloid should return coordinates" errors))
            (when (listp coords)
              (unless (= (length coords) 2)
                (push "Epicycloid should return (x, y)" errors))))
          
          ;; Test epicycloid at t=π
          (let ((coords (meta-log-epicycloid-parametrization pi 5 2)))
            (unless (listp coords)
              (push "Epicycloid at π should work" errors)))
          
          ;; Test winding number
          (let ((winding (meta-log-epicycloid-winding-number 5 2)))
            (unless (numberp winding)
              (push "Winding number should be a number" errors))
            (when (numberp winding)
              ;; Winding = (R+r)/r = 7/2 = 3.5
              (unless (< (abs (- winding 3.5)) 0.1)
                (push (format "Winding number should be 3.5, got %f" winding) errors)))))
      (error (push (format "Epicycloid error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Epicycloid tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Epicycloid tests passed")
        t))))

(defun test-geometric-rosette ()
  "Test rosette curves."
  (message "Testing rosette parametrization...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test rosette with k=2
          (let ((coords (meta-log-rosette-parametrization 0.0 2)))
            (unless (listp coords)
              (push "Rosette should return coordinates" errors))
            (when (listp coords)
              (unless (= (length coords) 2)
                (push "Rosette should return (x, y)" errors))))
          
          ;; Test rosette with k=3
          (let ((coords (meta-log-rosette-parametrization (/ pi 2.0) 3)))
            (unless (listp coords)
              (push "Rosette with k=3 should work" errors))
            (when (listp coords)
              (unless (= (length coords) 2)
                (push "Rosette with k=3 should return (x, y)" errors))))
          
          ;; Test petal count
          (let ((petals (meta-log-rosette-petal-count 2)))
            (unless (= petals 4)
              (push (format "Rosette with k=2 should have 4 petals, got %d" petals) errors)))
          (let ((petals (meta-log-rosette-petal-count 3)))
            (unless (= petals 3)
              (push (format "Rosette with k=3 should have 3 petals, got %d" petals) errors)))
      (error (push (format "Rosette error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Rosette tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Rosette tests passed")
        t))))

(defun test-geometric-p-adic-variants ()
  "Test p-adic geometric variants."
  (message "Testing p-adic geometric variants...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test p-adic rosette
          (let ((rosette (meta-log-p-adic-rosette '(0 0) 4 2)))
            (unless (functionp rosette)
              (push "p-adic rosette should be a function" errors))
            (when (functionp rosette)
              (let ((coords (funcall rosette 0.0)))
                (unless (listp coords)
                  (push "p-adic rosette should return coordinates" errors))
                (when (listp coords)
                  (unless (= (length coords) 2)
                    (push "p-adic rosette should return (x, y)" errors))))))
          
          ;; Test p-adic deltoid
          (let ((cusps '((0 0) (1 0) (0 1))))
            (let ((deltoid (meta-log-p-adic-deltoid cusps 2)))
              (unless deltoid
                (push "p-adic deltoid should return result" errors))
              (when deltoid
                (unless (eq (cdr (assq :type deltoid)) 'p-adic-deltoid)
                  (push "p-adic deltoid should have correct type" errors)))))
          
          ;; Test p-adic epicycloid
          (let ((epicycloid (meta-log-p-adic-epicycloid '(5 2) 2)))
            (unless (functionp epicycloid)
              (push "p-adic epicycloid should be a function" errors))
            (when (functionp epicycloid)
              (let ((result (funcall epicycloid 0.0)))
                (unless result
                  (push "p-adic epicycloid should return result" errors))
                (when result
                  (unless (listp result)
                    (push "p-adic epicycloid should return list" errors)))))))
      (error (push (format "p-adic geometric variants error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ p-adic geometric variants tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ p-adic geometric variants tests passed")
        t))))

(defun test-geometric-alignments-classify ()
  "Test geometric alignments classification."
  (message "Testing geometric alignments classification...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test deltoid classification
          (let ((result (meta-log-geometric-alignments-classify :deltoid "3D")))
            (unless result
              (push "Deltoid classification should return result" errors))
            (when result
              (unless (eq (cdr (assq :type result)) 'deltoid)
                (push "Classification should have correct type" errors))))
          
          ;; Test astroid classification
          (let ((result (meta-log-geometric-alignments-classify :astroid "4D")))
            (unless result
              (push "Astroid classification should return result" errors)))
          
          ;; Test epicycloid classification
          (let ((result (meta-log-geometric-alignments-classify :epicycloid "11D")))
            (unless result
              (push "Epicycloid classification should return result" errors))))
      (error (push (format "Geometric alignments classification error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Geometric alignments classification tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Geometric alignments classification tests passed")
        t))))

(defun test-geometric-alignments-all ()
  "Run all geometric alignments tests."
  (message "=== Running Geometric Alignments Test Suite ===")
  (let ((results '()))
    (push (test-geometric-deltoid) results)
    (push (test-geometric-astroid) results)
    (push (test-geometric-epicycloid) results)
    (push (test-geometric-rosette) results)
    (push (test-geometric-p-adic-variants) results)
    (push (test-geometric-alignments-classify) results)
    
    (let ((passed (length (cl-remove-if-not 'identity results)))
          (total (length results)))
      (message "")
      (message "=== Test Results ===")
      (message "Passed: %d/%d" passed total)
      (if (= passed total)
          (progn
            (message "✓ All geometric alignments tests passed!")
            t)
        (progn
          (message "✗ Some tests failed")
          nil))))))

(provide 'test-geometric-alignments)
;;; test-geometric-alignments.el ends here

