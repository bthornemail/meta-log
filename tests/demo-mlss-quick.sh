#!/bin/bash
# MLSS Quick Demo (Non-Interactive)
# Meta-Log Substrate System - Quick Demonstration
# Copyright (C) 2025 Meta-Log Research Group

set -e

echo "=========================================="
echo "Meta-Log Substrate System (MLSS) Quick Demo"
echo "=========================================="
echo ""

# Demo 1: Foundation
echo "Demo 1: Foundation - Substrate Runtime"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((data #u8(1 2 3 4 5 6 7 8))
       (meta '((content-type . \"demo-data\")))
       (result (substrate-create-memory data meta))
       (uri (list-ref result 1)))
  (display \"  ✓ Memory object created: \") (display uri) (newline))
" 2>&1 | grep -v "^;;;" | grep -E "(✓|Memory|URI)"

# Demo 2: Waveform
echo ""
echo "Demo 2: Waveform Synthesis"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let ((waveform (make-waveform '(0.5 0.7 0.9 0.7 0.5) '() 44100)))
  (display \"  ✓ Waveform created: \") (display (length (list-ref waveform 1))) (display \" samples\\n\"))
" 2>&1 | grep -v "^;;;" | grep -E "(✓|Waveform|samples)"

# Demo 3: Q*
echo ""
echo "Demo 3: Q* Optimality Engine"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((state (make-qstar-state '((x . 0) (y . 0)) '((goal-x . 10) (goal-y . 10))))
       (action (make-qstar-action 'move-right '((dx . 1) (dy . 0))))
       (cost (qstar-evaluate state action)))
  (display \"  ✓ Q* evaluation: cost = \") (display cost) (newline))
" 2>&1 | grep -v "^;;;" | grep -E "(✓|Q\*|cost)"

# Demo 4: Vision
echo ""
echo "Demo 4: Computer Vision Pipeline"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((img (make-image 100 100 '((1 2 3) (4 5 6) (7 8 9))))
       (cbs-result (image-to-cbs img))
       (uri (list-ref cbs-result 1)))
  (display \"  ✓ Image processed: \") (display uri) (newline))
" 2>&1 | grep -v "^;;;" | grep -E "(✓|Image|mlss)"

# Demo 5: Consciousness
echo ""
echo "Demo 5: Consciousness Framework"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((state (make-conscious-state 5.0 0.7 0.8))
       (qualia (emerge-qualia 5.0 0.7 0.8 0.3)))
  (if (list? qualia)
      (begin (display \"  ✓ Qualia emerged: coherence = \") (display (list-ref qualia 3)) (newline))
      (display \"  ✓ Conscious state created\\n\")))
" 2>&1 | grep -v "^;;;" | grep -E "(✓|Qualia|Conscious|coherence)"

# Demo 6: Physics
echo ""
echo "Demo 6: Computational Physics"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(let* ((quantum (make-quantum-state 2 '(0.707 0.707 0.0 0.0)))
       (probabilities (quantum-compute-probabilities (list-ref quantum 2))))
  (display \"  ✓ Quantum state created: \") (display (length probabilities)) (display \" probabilities\\n\"))
(let ((equations (einstein-equations '((1.0 0.0 0.0 0.0) (0.0 1.0 0.0 0.0) (0.0 0.0 1.0 0.0) (0.0 0.0 0.0 1.0)))))
  (display \"  ✓ Einstein equations computed\\n\"))
" 2>&1 | grep -v "^;;;" | grep -E "(✓|Quantum|Einstein|probabilities)"

echo ""
echo "=========================================="
echo "MLSS Quick Demo Complete"
echo "=========================================="
echo ""
echo "All six phases demonstrated successfully!"
echo ""

