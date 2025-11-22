---
layout: default
title: Binary Quadratic Forms (BQF)
nav_order: 10
description: "Binary Quadratic Forms and their discriminant applications"
permalink: /research/binary-forms
---

# Binary Quadratic Forms (BQF)

## Overview

Binary Quadratic Forms (BQFs) are homogeneous polynomials q(x, y) = ax² + bxy + cy² with integer coefficients. The discriminant Δ = b² - 4ac is a key invariant that classifies computational dualities.

## The Discriminant

The discriminant Δ = b² - 4ac classifies forms as:

- **Positive definite** (Δ < 0, a > 0): Represents only positive integers; finite automorphisms
- **Negative definite** (Δ < 0, a < 0): Represents only negative integers
- **Indefinite** (Δ > 0): Represents both signs; infinite cyclic automorphisms
- **Degenerate** (Δ perfect square): Represents zero non-trivially

## Computational Applications

In the metaverse, BQF discriminant classifies:

### Eager vs. Lazy Evaluation
- **Definite Δ** (Δ < 0): Eager evaluation, finite automorphisms
- **Indefinite Δ** (Δ > 0): Lazy evaluation, infinite behaviors

### Data vs. Codata
- **Definite**: Data structures (construction)
- **Indefinite**: Codata structures (observation)

### Construction vs. Observation
- **Definite**: Left adjoint (construction)
- **Indefinite**: Right adjoint (observation)

## Applications in the Metaverse

### 1. Election Stability
- **Definite forms**: Stable quorums in A11 elections
- **Indefinite forms**: Re-elections under network partitions (β₀ > 1)

### 2. Epistemic Observability
- **Definite Δ**: Stable UK·φ(V) observability
- **Indefinite Δ**: Lazy horizons, triggers re-elections

### 3. FRBAC Delegation
- **Definite Δ**: Verifiable hierarchies in 8D manifold
- **Indefinite Δ**: Flags "lazy" epistemic fog

### 4. Homology Checking
- **6D Homology Checker**: Uses Δ to classify chains
- **Definite**: Coherent epistemic states (KK-dominant)
- **Indefinite**: Invalid homology, triggers ML flags

## Number Theory Applications

### Class Numbers
The number of reduced primitive forms for a fixed Δ equals the class number of the quadratic field ℚ(√Δ).

### Integer Representations
A form represents an integer n if n = q(x,y) for integers x,y. For primes p, a form of Δ represents p iff Δ is a quadratic residue mod p.

### Composition
Composition combines forms of the same Δ, inducing a group operation on classes.

## Cryptographic Applications

BQF discriminants underpin public-key cryptography:
- **Class Group Cryptosystems**: Computing discrete logs in class groups is hard
- **Security Foundations**: Used in schemes like CL signatures or verifiable delay functions
- **Quantum Resistance**: Partially resistant via isogenies

## References

- [Trinary Forms](trinary-forms.md) - Extension to 3D
- [Quaternary Forms](quaternary-forms.md) - Extension to 4D
- [Research: 16-bqf](../dev-docs/research/16-bqf.md) - Complete BQF document

