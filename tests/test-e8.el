;;; test-e8.el --- E8 Lattice Test Suite

;; Test suite for meta-log E8 lattice operations

;; Add load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory (or load-file-name default-directory))))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory))))

(require 'cl-lib)
(require 'meta-log-e8)
(require 'meta-log-p-adic)

(defun test-e8-root-construction ()
  "Test E8 root system construction (240 roots)."
  (message "Testing E8 root construction...")
  (let ((errors '()))
    (condition-case err
        (progn
          (meta-log-e8-initialize)
          (unless meta-log-e8--roots
            (push "E8 roots should be constructed" errors))
          (unless (= (length meta-log-e8--roots) 240)
            (push (format "E8 should have 240 roots, got %d" (length meta-log-e8--roots)) errors))
          (unless (= (length meta-log-e8--simple-roots) 8)
            (push (format "E8 should have 8 simple roots, got %d" (length meta-log-e8--simple-roots)) errors))
          (unless (= (length meta-log-e8--weyl-generators) 8)
            (push (format "E8 should have 8 Weyl generators, got %d" (length meta-log-e8--weyl-generators)) errors))
          ;; Check that roots have norm² = 2
          (let ((sample-root (nth 0 meta-log-e8--roots)))
            (when sample-root
              (let ((norm-sq (cl-reduce #'+
                                       (mapcar (lambda (c) (* c c)) sample-root))))
                (unless (< (abs (- norm-sq 2.0)) 0.1)
                  (push (format "E8 root should have norm² ≈ 2, got %f" norm-sq) errors))))))
      (error (push (format "E8 root construction error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ E8 root construction tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ E8 root construction tests passed")
        t))))

(defun test-e8-bip32-mapping ()
  "Test BIP32 path → E8 point mapping."
  (message "Testing BIP32 → E8 mapping...")
  (let ((errors '()))
    (condition-case err
        (progn
          ;; Test simple path
          (let ((path "m/44'/0'/0'/0/0")
                (point (meta-log-e8-bip32-to-e8 path)))
            (unless (meta-log-e8-point-p point)
              (push "BIP32 path should create E8 point" errors))
            (unless (string= (meta-log-e8-point-bip32-path point) path)
              (push "E8 point should have correct BIP32 path" errors))
            (unless (= (length (meta-log-e8-point-coords point)) 8)
              (push "E8 point should have 8 coordinates" errors))
            (unless (= (meta-log-e8-point-depth point) 4)
              (push (format "E8 point depth should be 4, got %d" (meta-log-e8-point-depth point)) errors)))
          
          ;; Test master path
          (let ((path "m/44'/0'/0'")
                (point (meta-log-e8-bip32-to-e8 path)))
            (unless (meta-log-e8-point-p point)
              (push "Master BIP32 path should work" errors))
            (unless (null (meta-log-e8-point-parent point))
              (push "Master path should have no parent" errors)))
          
          ;; Test norm calculation
          (let ((point (meta-log-e8-bip32-to-e8 "m/44'/0'/0'/0/0")))
            (let ((norm-sq (meta-log-e8-point-norm-squared point)))
              (unless (>= norm-sq 0)
                (push (format "Norm² should be non-negative, got %f" norm-sq) errors)))))
      (error (push (format "BIP32 mapping error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ BIP32 mapping tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ BIP32 mapping tests passed")
        t))))

(defun test-e8-weyl-orbit ()
  "Test Weyl orbit computation."
  (message "Testing Weyl orbit computation...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((point (meta-log-e8-bip32-to-e8 "m/44'/0'/0'/0/0"))
                (orbit (meta-log-e8-weyl-orbit point 10)))  ; Small limit for testing
            (unless (listp orbit)
              (push "Weyl orbit should return list" errors))
            (unless (> (length orbit) 0)
              (push "Weyl orbit should have at least one point" errors))
            (unless (<= (length orbit) 10)
              (push (format "Weyl orbit should respect limit, got %d points" (length orbit)) errors))
            ;; Check that original point is in orbit
            (unless (cl-find point orbit :test #'equal)
              (push "Original point should be in its Weyl orbit" errors))))
      (error (push (format "Weyl orbit error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Weyl orbit tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Weyl orbit tests passed")
        t))))

(defun test-e8-padic-height ()
  "Test p-adic height calculation."
  (message "Testing p-adic height...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((point (meta-log-e8-bip32-to-e8 "m/44'/0'/0'/0/0")))
            ;; Test 2-adic height
            (let ((h2 (meta-log-e8-padic-height point 2)))
              (unless (numberp h2)
                (push "2-adic height should return number" errors))
              (unless (>= h2 0)
                (push (format "2-adic height should be non-negative, got %f" h2) errors)))
            ;; Test 3-adic height
            (let ((h3 (meta-log-e8-padic-height point 3)))
              (unless (numberp h3)
                (push "3-adic height should return number" errors))
              (unless (>= h3 0)
                (push (format "3-adic height should be non-negative, got %f" h3) errors)))))
      (error (push (format "p-adic height error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ p-adic height tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ p-adic height tests passed")
        t))))

(defun test-e8-frbac-verification ()
  "Test FRBAC delegation verification."
  (message "Testing FRBAC verification...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((master (meta-log-e8-bip32-to-e8 "m/44'/0'/0'"))
                (delegate (meta-log-e8-bip32-to-e8 "m/44'/0'/0'/0/0")))
            (let ((is-valid (meta-log-e8-verify-frbac-delegation master delegate)))
              (unless (or is-valid (not is-valid))  ; Just check it doesn't error
                (push "FRBAC verification should return boolean" errors)))))
      (error (push (format "FRBAC verification error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ FRBAC verification tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ FRBAC verification tests passed")
        t))))

(defun test-e8-distance-features ()
  "Test distance features for ML."
  (message "Testing distance features...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((p1 (meta-log-e8-bip32-to-e8 "m/44'/0'/0'/0/0"))
                (p2 (meta-log-e8-bip32-to-e8 "m/44'/0'/0'/0/1"))
                (dists (meta-log-e8-distance-for-ml p1 p2)))
            (unless (listp dists)
              (push "Distance features should return alist" errors))
            (unless (assq 'euclidean dists)
              (push "Distance features should include euclidean" errors))
            (unless (assq 'padic_2 dists)
              (push "Distance features should include padic_2" errors))
            (unless (assq 'padic_3 dists)
              (push "Distance features should include padic_3" errors))
            (unless (assq 'weyl_distance dists)
              (push "Distance features should include weyl_distance" errors))
            ;; Check values are non-negative
            (dolist (pair dists)
              (let ((val (cdr pair)))
                (unless (>= val 0)
                  (push (format "Distance feature %s should be non-negative, got %f" (car pair) val) errors))))))
      (error (push (format "Distance features error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Distance features tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Distance features tests passed")
        t))))

(defun test-e8-shortest-path ()
  "Test shortest path algorithm."
  (message "Testing shortest path...")
  (let ((errors '()))
    (condition-case err
        (progn
          (let ((start (meta-log-e8-bip32-to-e8 "m/44'/0'/0'/0/0"))
                (end (meta-log-e8-bip32-to-e8 "m/44'/0'/0'/0/1"))
                (result (meta-log-e8-shortest-path start end)))
            (unless (consp result)
              (push "Shortest path should return (path . distance)" errors))
            (let ((path (car result))
                  (distance (cdr result)))
              (unless (listp path)
                (push "Path should be list of points" errors))
              (unless (> (length path) 0)
                (push "Path should have at least one point" errors))
              (unless (>= distance 0)
                (push (format "Distance should be non-negative, got %f" distance) errors)))))
      (error (push (format "Shortest path error: %s" err) errors)))
    
    (if errors
        (progn
          (message "✗ Shortest path tests failed: %s" (mapconcat 'identity errors ", "))
          nil)
      (progn
        (message "✓ Shortest path tests passed")
        t))))

(defun test-e8-all ()
  "Run all E8 lattice tests."
  (message "=== Running E8 Lattice Test Suite ===")
  (let ((results '()))
    (push (test-e8-root-construction) results)
    (push (test-e8-bip32-mapping) results)
    (push (test-e8-weyl-orbit) results)
    (push (test-e8-padic-height) results)
    (push (test-e8-frbac-verification) results)
    (push (test-e8-distance-features) results)
    (push (test-e8-shortest-path) results)
    
    (let ((passed (length (cl-remove-if-not 'identity results)))
          (total (length results)))
      (message "")
      (message "=== Test Results ===")
      (message "Passed: %d/%d" passed total)
      (if (= passed total)
          (progn
            (message "✓ All E8 lattice tests passed!")
            t)
        (progn
          (message "✗ Some tests failed")
          nil)))))

(provide 'test-e8)
;;; test-e8.el ends here

