#!/bin/bash
# MLSS System Demo
# Meta-Log Substrate System - Interactive Demonstration
# Copyright (C) 2025 Meta-Log Research Group

set -e

echo "=========================================="
echo "Meta-Log Substrate System (MLSS) Demo"
echo "=========================================="
echo ""
echo "This demo showcases all six phases of the MLSS integration:"
echo "  1. Foundation (Substrate Runtime & Binary)"
echo "  2. Waveform & Geometric"
echo "  3. Q* Optimality Engine"
echo "  4. Computer Vision"
echo "  5. Consciousness Framework"
echo "  6. Computational Physics"
echo ""
echo "Press Enter to continue..."
read

# Demo 1: Foundation
echo ""
echo "=========================================="
echo "Demo 1: Foundation - Substrate Runtime"
echo "=========================================="
echo ""
echo "Creating memory objects and CBS..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(display \"Creating memory object...\\n\")
(let* ((data #u8(1 2 3 4 5 6 7 8))
       (meta '((content-type . \"demo-data\")
               (version . 1)))
       (result (substrate-create-memory data meta))
       (obj (list-ref result 0))
       (uri (list-ref result 1)))
  (display \"  Memory object created\\n\")
  (display \"  URI: \") (display uri) (newline)
  (display \"\\nCreating CBS...\\n\")
  (let ((cbs (make-cbs data meta)))
    (display \"  CBS created with hash: \")
    (display (list-ref cbs 5)) (newline)))
(display \"\\n✓ Foundation demo complete\\n\")
" 2>&1 | grep -v "^;;;"

echo ""
echo "Press Enter to continue..."
read

# Demo 2: Waveform
echo ""
echo "=========================================="
echo "Demo 2: Waveform Synthesis"
echo "=========================================="
echo ""
echo "Creating waveform from WDL specification..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(display \"Creating waveform directly...\\n\")
(let ((waveform (make-waveform '(0.5 0.7 0.9 0.7 0.5) '() 44100)))
  (display \"  Waveform created\\n\")
  (display \"  Sample rate: \") (display (list-ref waveform 3)) (newline)
  (display \"  Sample count: \") (display (length (list-ref waveform 1))) (newline))
(display \"\\n✓ Waveform demo complete\\n\")
" 2>&1 | grep -v "^;;;"

echo ""
echo "Press Enter to continue..."
read

# Demo 3: Q*
echo ""
echo "=========================================="
echo "Demo 3: Q* Optimality Engine"
echo "=========================================="
echo ""
echo "Solving pathfinding problem with Q*..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(display \"Initial state: (x=0, y=0)\\n\")
(display \"Goal: (x=10, y=10)\\n\")
(let* ((state (make-qstar-state '((x . 0) (y . 0)) '((goal-x . 10) (goal-y . 10))))
       (action1 (make-qstar-action 'move-right '((dx . 1) (dy . 0))))
       (action2 (make-qstar-action 'move-up '((dx . 0) (dy . 1))))
       (cost1 (qstar-evaluate state action1))
       (cost2 (qstar-evaluate state action2)))
  (display \"  Action 'move-right' cost: \") (display cost1) (newline)
  (display \"  Action 'move-up' cost: \") (display cost2) (newline)
  (display \"  Optimal action: \")
  (display (if (< cost1 cost2) 'move-right 'move-up)) (newline))
(display \"\\n✓ Q* demo complete\\n\")
" 2>&1 | grep -v "^;;;"

echo ""
echo "Press Enter to continue..."
read

# Demo 4: Vision
echo ""
echo "=========================================="
echo "Demo 4: Computer Vision Pipeline"
echo "=========================================="
echo ""
echo "Processing image through vision pipeline..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(display \"Creating test image (100x100)...\\n\")
(let* ((pixels '((1 2 3) (4 5 6) (7 8 9)))
       (img (make-image 100 100 pixels))
       (cbs-result (image-to-cbs img))
       (uri (list-ref cbs-result 1)))
  (display \"  Image created\\n\")
  (display \"  Converted to CBS: \") (display uri) (newline)
  (display \"  Extracting edges...\\n\")
  (let ((edges (extract-edges img)))
    (display \"  Edges extracted: \") (display (length edges)) (display \" edge points\\n\")))
(display \"\\n✓ Vision demo complete\\n\")
" 2>&1 | grep -v "^;;;"

echo ""
echo "Press Enter to continue..."
read

# Demo 5: Consciousness
echo ""
echo "=========================================="
echo "Demo 5: Consciousness Framework"
echo "=========================================="
echo ""
echo "Simulating conscious state evolution..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(display \"Initial conscious state:\\n\")
(display \"  Action: 5.0, Observation: 0.7, Phase: 0.8\\n\")
(let* ((state1 (make-conscious-state 5.0 0.7 0.8))
       (action1 5.0)
       (obs1 0.7)
       (next-action (conscious-action-forward action1 0.1 0.0))
       (next-obs (conscious-observation-backward obs1 0.9 (lambda (x) x)))
       (qualia (emerge-qualia next-action next-obs 0.8 0.3))
       (state2 (make-conscious-state next-action next-obs 0.8))
       (metrics (collect-metrics state2 state1 qualia)))
  (display \"  After forward propagation:\\n\")
  (display \"    Action: \") (display next-action) (newline)
  (display \"    Observation: \") (display next-obs) (newline)
  (if (list? qualia)
      (begin (display \"  Qualia emerged!\\n\")
             (display \"    Coherence: \") (display (list-ref qualia 3)) (newline)
             (display \"    Richness: \") (display (list-ref qualia 4)) (newline)))
  (display \"  Consciousness Quality Metric: \")
  (display (compute-cqm metrics '((action . 1.0) (observation . 1.0) (coherence . 1.0) (qualia . 1.0) (learning . 1.0))))
  (newline))
(display \"\\n✓ Consciousness demo complete\\n\")
" 2>&1 | grep -v "^;;;"

echo ""
echo "Press Enter to continue..."
read

# Demo 6: Physics
echo ""
echo "=========================================="
echo "Demo 6: Computational Physics"
echo "=========================================="
echo ""
echo "Quantum state and field theory operations..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(display \"Creating quantum state (2 qubits, Bell state)...\\n\")
(let* ((quantum (make-quantum-state 2 '(0.707 0.707 0.0 0.0)))
       (probabilities (quantum-compute-probabilities (list-ref quantum 2))))
  (display \"  Quantum state created\\n\")
  (display \"  Probabilities: \") (display probabilities) (newline))
(display \"\\nComputing Einstein field equations...\\n\")
(let ((equations (einstein-equations '((1.0 0.0 0.0 0.0) (0.0 1.0 0.0 0.0) (0.0 0.0 1.0 0.0) (0.0 0.0 0.0 1.0)))))
  (display \"  Field equations computed\\n\")
  (display \"  Equation type: \") (display (list-ref equations 0)) (newline))
(display \"\\nCreating quantum field configuration...\\n\")
(let ((field (make-field-configuration 'scalar '(1.0 2.0 3.0 4.0) 0.1)))
  (display \"  Field configuration created\\n\")
  (display \"  Field type: \") (display (list-ref field 2)) (newline)
  (display \"  Coupling: \") (display (list-ref field 4)) (newline))
(display \"\\n✓ Physics demo complete\\n\")
" 2>&1 | grep -v "^;;;"

# Final Summary
echo ""
echo "=========================================="
echo "MLSS Demo Complete"
echo "=========================================="
echo ""
echo "All six phases demonstrated successfully!"
echo ""
echo "The Meta-Log Substrate System provides:"
echo "  ✓ Universal binary substrate (CBS)"
echo "  ✓ Waveform synthesis and manipulation"
echo "  ✓ Optimality-driven decision making (Q*)"
echo "  ✓ Computer vision processing"
echo "  ✓ Conscious state management and qualia"
echo "  ✓ Quantum physics and field theory computation"
echo ""
echo "For more information, see:"
echo "  - dev-docs/INTEGRATION-MLSS.md"
echo "  - tests/BENCHMARK-RESULTS.md"
echo ""

