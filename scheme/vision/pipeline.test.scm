;;; vision/pipeline.test.scm --- Vision Pipeline Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for vision processing pipeline

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../substrate/binary.scm")
(load "../vision/pipeline.scm")

;; Test 1: Image Creation
(display "Test 1: Image Creation\n")
(let* ((width 10)
       (height 10)
       (channels 1)
       (pixels (make-list (* width height) 128))
       (image (make-image width height channels pixels)))
  (if (and (list? image)
           (= (length image) 7)
           (eq? (list-ref image 0) 'image)
           (= (list-ref image 2) width)
           (= (list-ref image 3) height))
      (display "  ✓ Image creation works\n")
      (begin (display "  ✗ Image creation failed\n") (exit 1))))

;; Test 2: Image → CBS Conversion
(display "Test 2: Image → CBS Conversion\n")
(let* ((width 5)
       (height 5)
       (channels 1)
       (pixels (make-list (* width height) 100))
       (image (make-image width height channels pixels))
       (cbs-result (image-to-cbs image))
       (cbs (list-ref cbs-result 0))
       (uri (list-ref cbs-result 1)))
  (let ((uri-prefix (if (and (string? uri) (>= (string-length uri) 7))
                        (substring uri 0 7)
                        "")))
    (if (and (list? cbs)
             (string? uri)
             (string=? uri-prefix "mlss://"))
        (display "  ✓ Image to CBS conversion works\n")
        (begin (display "  ✗ Image to CBS conversion failed\n") (exit 1)))))

;; Test 3: Edge Extraction
(display "Test 3: Edge Extraction\n")
(let* ((width 10)
       (height 10)
       (channels 1)
       (pixels (make-list (* width height) 128))
       (image (make-image width height channels pixels))
       (edges (extract-edges image 5.0)))
  (if (list? edges)
      (display "  ✓ Edge extraction works\n")
      (begin (display "  ✗ Edge extraction failed\n") (exit 1))))

;; Test 4: Features → E8 Projection
(display "Test 4: Features → E8 Projection\n")
(let* ((features '((5 5 10.0) (7 7 15.0) (3 3 8.0)))
       (e8-result (features-to-e8 features 'spectral '())))
  (if (and (list? e8-result)
           (eq? (list-ref e8-result 0) 'e8-vector))
      (display "  ✓ Features to E8 projection works\n")
      (begin (display "  ✗ Features to E8 projection failed\n") (exit 1))))

(display "\nAll vision pipeline tests passed!\n")

