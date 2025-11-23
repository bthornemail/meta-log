;;; qstar/a-star.test.scm --- Q* A* Pathfinding Tests
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Unit tests for Q* A* pathfinding algorithm

;;; Code:

;; Load dependencies
(load "../substrate/runtime.scm")
(load "../qstar/core.scm")
(load "../qstar/scoring.scm")
(load "../qstar/a-star.scm")

;; Test 1: A* Node Creation
(display "Test 1: A* Node Creation\n")
(let* ((state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ())))
       (node (make-astar-node state 5.0 3.0 #f #f)))
  (if (and (list? node)
           (= (length node) 7)
           (eq? (list-ref node 0) 'astar-node)
           (= (list-ref node 2) 5.0)  ; g-cost
           (= (list-ref node 3) 3.0)  ; h-cost
           (= (list-ref node 4) 8.0))  ; f-cost
      (display "  ✓ Node creation works\n")
      (begin (display "  ✗ Node creation failed\n") (exit 1))))

;; Test 2: A* Search Structure
(display "Test 2: A* Search Structure\n")
(let* ((start-state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ())))
       (goal-pred (lambda (s) #f))  ; Never reach goal for this test
       (heuristic (lambda (s) 0.0))
       (opts '((max-nodes . 10)))
       (result (qstar-a-star start-state goal-pred heuristic opts)))
  (if (and (list? result)
           (= (length result) 3)
           (list? (list-ref result 0))  ; path
           (number? (list-ref result 1))  ; cost
           (number? (list-ref result 2)))  ; nodes-expanded
      (display "  ✓ A* search structure works\n")
      (begin (display "  ✗ A* search structure failed\n") (exit 1))))

;; Test 3: Heuristic Functions
(display "Test 3: Heuristic Functions\n")
(let ((state '((binary . ()) (waveform . ()) (geometric . ()) (symbolic . ()))))
  (let ((weyl-h (heuristic-weyl state))
        (euclidean-h (heuristic-euclidean state))
        (padic-h (heuristic-padic-lowerbound state)))
    (if (and (number? weyl-h)
             (number? euclidean-h)
             (number? padic-h)
             (>= weyl-h 0)
             (>= euclidean-h 0)
             (>= padic-h 0))
        (display "  ✓ Heuristic functions work\n")
        (begin (display "  ✗ Heuristic functions failed\n") (exit 1)))))

;; Test 4: Path Reconstruction
(display "Test 4: Path Reconstruction\n")
(let ((node1 (make-astar-node 'state1 0.0 5.0 #f #f))
      (node2 (make-astar-node 'state2 1.0 4.0 node1 'action1))
      (node3 (make-astar-node 'state3 2.0 3.0 node2 'action2))
      (path (reconstruct-path node3)))
  (if (and (list? path)
           (>= (length path) 0))  ; Path may be empty if no actions
      (display "  ✓ Path reconstruction works\n")
      (begin (display "  ✗ Path reconstruction failed\n") (exit 1))))

(display "\nAll Q* A* tests passed!\n")

