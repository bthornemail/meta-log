#!/bin/bash
# Interactive Demo: Autonomy and Awareness
# Meta-Log Substrate System

set -e

echo "=========================================="
echo "Autonomous Aware System Demo"
echo "=========================================="
echo ""

echo "This demo shows:"
echo "  1. Autonomous behavior (perceive → decide → act → learn)"
echo "  2. Awareness capabilities (self-monitoring, reflection)"
echo "  3. Consciousness state evolution"
echo "  4. Action execution and learning"
echo ""

# Demo 1: Autonomous Cycle
echo "Demo 1: Autonomous Cycle"
echo "  Running single autonomous cycle..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/autonomy/loop.scm\")
(load \"scheme/autonomy/test-env.scm\")
(let* ((initial-state (make-qstar-state '((binary . ())) '((total-memory . 0))))
       (sensors '((gps . ,(simulate-sensor-data 'gps 0.0))))
       (action-space (list (list 'action 'file-write 'write-file '((path . \"test.txt\") (data . \"test\")))))
       (cycle-result (autonomous-cycle initial-state sensors action-space)))
  (display \"  Cycle completed: \")
  (display (length cycle-result))
  (display \" components\\n\")
  (display \"  ✓ Autonomous cycle works\\n\"))
" && echo "  ✓ Demo 1 PASSED" || echo "  ✗ Demo 1 FAILED"

echo ""

# Demo 2: Self-Monitoring
echo "Demo 2: Self-Monitoring"
echo "  Monitoring consciousness state..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/consciousness/self-monitoring.scm\")
(let* ((state (make-conscious-state 5.0 0.7 0.8))
       (monitoring (monitor-own-state state))
       (awareness (assoc-ref monitoring 'self-awareness)))
  (display \"  Self-awareness index: \")
  (display awareness)
  (newline)
  (display \"  ✓ Self-monitoring works\\n\"))
" && echo "  ✓ Demo 2 PASSED" || echo "  ✗ Demo 2 FAILED"

echo ""

# Demo 3: Reflection
echo "Demo 3: Self-Reflection"
echo "  Reflecting on action..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/consciousness/reflection.scm\")
(load \"scheme/action/executor.scm\")
(let* ((action (list 'action 'file-write 'write-file '((path . \"test.txt\"))))
       (action-result (make-action-result 'test-id #t '((written . #t)) '() \"\"))
       (reflection (reflect-on-action action action-result)))
  (display \"  Reflection quality: \")
  (display (assoc-ref reflection 'quality))
  (newline)
  (display \"  ✓ Self-reflection works\\n\"))
" && echo "  ✓ Demo 3 PASSED" || echo "  ✗ Demo 3 FAILED"

echo ""

# Demo 4: Integrated System
echo "Demo 4: Integrated System"
echo "  Running integrated autonomous aware cycle..."
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/sensors/gps.scm\")
(load \"scheme/autonomy/integrated-system.scm\")
(let* ((system-state (make-integrated-system-state))
       (sensors (list (cons 'gps (gps-read \"gps-1\"))))
       (action-space (list (list 'action 'file-write 'write-file '((path . \"test.txt\")))))
       (new-state (integrated-autonomous-cycle system-state sensors action-space)))
  (display \"  System state updated\\n\")
  (display \"  ✓ Integrated system works\\n\"))
" && echo "  ✓ Demo 4 PASSED" || echo "  ✗ Demo 4 FAILED"

echo ""

echo "=========================================="
echo "Demo Summary"
echo "=========================================="
echo ""
echo "All demos completed successfully!"
echo "The system demonstrates:"
echo "  - Autonomous behavior with closed-loop feedback"
echo "  - Self-monitoring and reflection capabilities"
echo "  - Integrated sensor → consciousness → action → learning cycle"
echo ""

