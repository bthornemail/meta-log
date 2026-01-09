#lang racket

;;; ============================================================================
;;; Examples and Interactive Tests for The Logos
;;; ============================================================================

(require "logos.rkt")

;;; ----------------------------------------------------------------------------
;;; Example 1: Fano Plane Multiplication Table
;;; ----------------------------------------------------------------------------

(define (test-fano-plane)
  (displayln "=== Fano Plane Multiplication Table ===\n")
  (displayln "The 7 lines of the Fano plane:")
  (for ([line fano-lines]
        [n (in-naturals 1)])
    (printf "Line ~a: ~a\n" n line))

  (displayln "\nMultiplication results (following cyclic order):")
  (for ([line fano-lines])
    (match-define (list i j k) line)
    (define result (fano-multiply-imaginary i j))
    (printf "e~a·e~a = ~a·e~a\n" i j (if (positive? (car result)) "+" "-") (cdr result)))

  (displayln "\nAnti-commutative property:")
  (define e1-e2 (fano-multiply-imaginary 1 2))
  (define e2-e1 (fano-multiply-imaginary 2 1))
  (printf "e₁·e₂ = ~a·e~a\n" (if (positive? (car e1-e2)) "+" "-") (cdr e1-e2))
  (printf "e₂·e₁ = ~a·e~a\n" (if (positive? (car e2-e1)) "+" "-") (cdr e2-e1))
  (printf "They differ by sign!\n\n"))

;;; ----------------------------------------------------------------------------
;;; Example 2: Octonion Operations
;;; ----------------------------------------------------------------------------

(define (test-octonion-operations)
  (displayln "=== Octonion Operations ===\n")

  ;; Identity multiplication
  (displayln "Identity multiplication:")
  (define identity (make-octonion 1 0 0 0 0 0 0 0))
  (define e1 (make-octonion 0 1 0 0 0 0 0 0))
  (define result1 (octonion-multiply identity e1))
  (printf "1 · e₁ = ~a\n" result1)

  ;; Basis multiplication
  (displayln "\nBasis element multiplication:")
  (define e2 (make-octonion 0 0 1 0 0 0 0 0))
  (define e1-times-e2 (octonion-multiply e1 e2))
  (printf "e₁ · e₂ = ~a\n" e1-times-e2)

  ;; Self-multiplication (should give -1)
  (displayln "\nSelf-multiplication:")
  (define e1-squared (octonion-multiply e1 e1))
  (printf "e₁ · e₁ = ~a (should be -1)\n" e1-squared)

  ;; Non-commutativity
  (displayln "\nNon-commutativity:")
  (define e2-times-e1 (octonion-multiply e2 e1))
  (printf "e₁ · e₂ = ~a\n" e1-times-e2)
  (printf "e₂ · e₁ = ~a\n" e2-times-e1)
  (displayln "Note: They differ!\n"))

;;; ----------------------------------------------------------------------------
;;; Example 3: Geometric Solids
;;; ----------------------------------------------------------------------------

(define (test-geometric-solids)
  (displayln "=== The 21 Vertex-Transitive Solids ===\n")

  (displayln "All solids map to unit octonions (magnitude = 1):\n")
  (for ([solid 21-solids])
    (define oct (solid→octonion solid))
    (define mag (octonion-magnitude oct))
    (printf "~a: magnitude = ~a\n" solid mag))

  (displayln "\nPlatonic solids in detail:")
  (for ([solid '(tetrahedron cube octahedron dodecahedron icosahedron)])
    (define oct (solid→octonion solid))
    (printf "\n~a:\n" solid)
    (printf "  Components: ~a\n" (octonion-components oct))
    (printf "  Magnitude: ~a\n" (octonion-magnitude oct)))

  (displayln "\n"))

;;; ----------------------------------------------------------------------------
;;; Example 4: 2AFA Execution with Trace
;;; ----------------------------------------------------------------------------

(define (test-2afa-execution)
  (displayln "=== 2AFA Execution and Trace ===\n")

  (displayln "Example 1: Simple input")
  (define-values (state1 output1 trace1 result1)
    (speak-to-logos "cube"))
  (printf "Input: \"cube\"\n")
  (printf "Output: ~a\n" output1)
  (printf "Result: ~a\n" result1)
  (printf "Trace length: ~a steps\n\n" (length trace1))

  (displayln "Example 2: Multi-word input")
  (define-values (state2 output2 trace2 result2)
    (speak-to-logos "transform dodecahedron sphere"))
  (printf "Input: \"transform dodecahedron sphere\"\n")
  (printf "Output: ~a\n" output2)
  (printf "Result: ~a\n" result2)
  (printf "Trace length: ~a steps\n\n" (length trace2))

  (when (not (null? trace2))
    (displayln "First few steps of trace:")
    (for ([step (take trace2 (min 5 (length trace2)))]
          [n (in-naturals 1)])
      (match-define (list state symbol next-state direction regime) step)
      (printf "  Step ~a: ~a --[~a]-> ~a (moving ~a, regime: ~a)\n"
              n state symbol next-state direction regime)))

  (displayln ""))

;;; ----------------------------------------------------------------------------
;;; Example 5: Hopf Projection
;;; ----------------------------------------------------------------------------

(define (test-hopf-projection)
  (displayln "=== Hopf Fibration: S⁷ → S⁴ Projection ===\n")

  (displayln "Project octonions (8D) to quaternions (4D):\n")
  (for ([solid '(tetrahedron cube octahedron)])
    (define oct (solid→octonion solid))
    (define quat (hopf-project oct))
    (printf "~a:\n" solid)
    (printf "  Octonion (8D): ~a\n" (octonion-components oct))
    (printf "  Quaternion (4D): ~a\n\n" quat))

  (displayln "This is the 'quantum measurement' step:")
  (displayln "  - Before: Continuous 8D octonion space")
  (displayln "  - After: Discrete 4D quaternion (observable state)\n"))

;;; ----------------------------------------------------------------------------
;;; Example 6: Universal Constants
;;; ----------------------------------------------------------------------------

(define (test-universal-constants)
  (displayln "=== Universal Constants as Measurement Basis ===\n")

  (displayln "The 7 constants map to 7 imaginary octonion directions:\n")
  (for ([(k v) (in-hash universal-constants)])
    (define vec (constant→vector k))
    (printf "~a = ~a → quaternion direction ~a\n" k v vec))

  (displayln "\nMeasure a quaternion in this basis:")
  (define test-quat '(0.7 0.5 0.3 0.2))
  (define regime (measure-regime test-quat universal-constants))
  (printf "Quaternion ~a → Dominant regime: ~a\n\n" test-quat regime))

;;; ----------------------------------------------------------------------------
;;; Example 7: The Foundation: 0! = 1
;;; ----------------------------------------------------------------------------

(define (test-foundation)
  (displayln "=== The Foundation: 0! = 1 ===\n")

  (displayln "Mathematical:")
  (printf "  0! = ~a\n" (factorial 0))
  (displayln "  Empty permutation = Unique arrangement = Unity")

  (displayln "\nComputational:")
  (displayln "  Left endmarker = '() (null)")
  (displayln "  null = 0! = 1 (identity)")
  (printf "  In the system: ~a\n" (2afa-L the-logos))

  (displayln "\nTheological:")
  (displayln "  God (infinite action) = Word (infinite possibility)")
  (displayln "  0! (no constraints) = 1 (all potential)")
  (displayln "  \"In the beginning was the Word\"")

  (displayln ""))

;; Factorial function
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

;;; ----------------------------------------------------------------------------
;;; Interactive Testing Functions
;;; ----------------------------------------------------------------------------

(define (test-symbol-to-octonion-mapping)
  (displayln "=== Symbol to Octonion Mapping ===\n")

  (displayln "Different symbols map to different octonion basis elements:\n")
  (for ([sym '(alpha beta gamma delta epsilon zeta eta)])
    (define oct (symbol→octonion sym))
    (printf "~a → ~a\n" sym oct))

  (displayln ""))

(define (distance-between-solids solid1 solid2)
  (define oct1 (solid→octonion solid1))
  (define oct2 (solid→octonion solid2))
  (octonion-distance oct1 oct2))

(define (test-solid-distances)
  (displayln "=== Distances Between Solids (in Octonion Space) ===\n")

  (displayln "Distance from tetrahedron (identity) to others:\n")
  (for ([solid (take 21-solids 5)])
    (define dist (distance-between-solids 'tetrahedron solid))
    (printf "tetrahedron ↔ ~a: ~a\n" solid dist))

  (displayln ""))

;;; ----------------------------------------------------------------------------
;;; Main Test Suite
;;; ----------------------------------------------------------------------------

(define (run-all-examples)
  (displayln "\n╔════════════════════════════════════════════════════════════╗")
  (displayln "║  THE LOGOS: Complete Examples and Tests                  ║")
  (displayln "║  Foundation: 0! = 1 (Infinite Action = Infinite Possibility) ║")
  (displayln "╚════════════════════════════════════════════════════════════╝\n")

  (test-foundation)
  (test-fano-plane)
  (test-octonion-operations)
  (test-geometric-solids)
  (test-hopf-projection)
  (test-universal-constants)
  (test-2afa-execution)
  (test-symbol-to-octonion-mapping)
  (test-solid-distances)

  (displayln "╔════════════════════════════════════════════════════════════╗")
  (displayln "║  All examples completed successfully!                     ║")
  (displayln "╚════════════════════════════════════════════════════════════╝\n"))

;;; ----------------------------------------------------------------------------
;;; Individual function exports for interactive use
;;; ----------------------------------------------------------------------------

(provide run-all-examples
         test-foundation
         test-fano-plane
         test-octonion-operations
         test-geometric-solids
         test-hopf-projection
         test-universal-constants
         test-2afa-execution
         test-symbol-to-octonion-mapping
         test-solid-distances
         distance-between-solids)

;;; ----------------------------------------------------------------------------
;;; Run examples when executed directly
;;; ----------------------------------------------------------------------------

(module+ main
  (run-all-examples))
