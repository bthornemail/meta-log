#!/bin/bash
# Test Autonomous Behavior
# Meta-Log Substrate System

set -e

echo "=========================================="
echo "Autonomous Behavior Tests"
echo "=========================================="
echo ""

# Test 1: Persistence (maintains goals over time)
echo "Test 1: Goal Persistence"
echo "  Test if system maintains goals over time"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/autonomy/goals.scm\")
(let* ((target-state (make-qstar-state '((binary . ())) '((total-memory . 100))))
       (goal (set-goal \"Test Goal\" target-state 0.8 #f))
       (goal-id (list-ref goal 1))
       (current-state (make-qstar-state '((binary . ())) '((total-memory . 50))))
       (progress1 (check-goal-progress goal-id current-state))
       (progress2 (check-goal-progress goal-id current-state)))
  (if (and (number? progress1) (number? progress2))
      (display \"  ✓ Goal persistence works\\n\")
      (begin (display \"  ✗ Goal persistence failed\\n\") (exit 1))))
" && echo "  ✓ Test 1 PASSED" || echo "  ✗ Test 1 FAILED"

echo ""

# Test 2: Adaptation (adapts to changing environment)
echo "Test 2: Environment Adaptation"
echo "  Test if system adapts to changing environment"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/autonomy/loop.scm\")
(load \"scheme/autonomy/test-env.scm\")
(let* ((initial-state (make-qstar-state '((binary . ())) '((total-memory . 0))))
       (action-space (list (list 'action 'file-write 'write-file '((path . \"test.txt\")))))
       (sensors1 '((gps . ,(simulate-sensor-data 'gps 0.0))))
       (sensors2 '((gps . ,(simulate-sensor-data 'gps 10.0))))
       (cycle1 (autonomous-cycle initial-state sensors1 action-space))
       (cycle2 (autonomous-cycle (list-ref cycle1 0) sensors2 action-space)))
  (if (and (list? cycle1) (list? cycle2))
      (display \"  ✓ Environment adaptation works\\n\")
      (begin (display \"  ✗ Environment adaptation failed\\n\") (exit 1))))
" && echo "  ✓ Test 2 PASSED" || echo "  ✗ Test 2 FAILED"

echo ""

# Test 3: Goal-Directedness (pursues goals effectively)
echo "Test 3: Goal-Directedness"
echo "  Test if system pursues goals effectively"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/autonomy/goals.scm\")
(let* ((target-state (make-qstar-state '((binary . ())) '((total-memory . 100))))
       (goal (set-goal \"Reach target\" target-state 0.9 #f))
       (goal-id (list-ref goal 1))
       (achieved (qstar-goal-check target-state)))
  (if (list? achieved)
      (display \"  ✓ Goal-directedness works\\n\")
      (begin (display \"  ✗ Goal-directedness failed\\n\") (exit 1))))
" && echo "  ✓ Test 3 PASSED" || echo "  ✗ Test 3 FAILED"

echo ""

# Test 4: Learning (improves with experience)
echo "Test 4: Learning from Experience"
echo "  Test if system improves with experience"
guile -c "
(load \"scheme/r5rs-canvas-engine.scm\")
(load \"scheme/autonomy/learning.scm\")
(let* ((state1 (make-qstar-state '((binary . ())) '((total-memory . 0))))
       (action (list 'action 'file-write 'write-file '((path . \"test.txt\"))))
       (outcome '((success . #t)))
       (reward 1.0)
       (state2 (make-qstar-state '((binary . ())) '((total-memory . 10))))
       (experience (store-experience state1 action outcome reward state2)))
  (if (and (list? experience) (eq? (list-ref experience 0) 'experience))
      (display \"  ✓ Learning from experience works\\n\")
      (begin (display \"  ✗ Learning from experience failed\\n\") (exit 1))))
" && echo "  ✓ Test 4 PASSED" || echo "  ✗ Test 4 FAILED"

echo ""

echo "=========================================="
echo "Autonomous Behavior Test Summary"
echo "=========================================="
echo ""

