---
layout: default
title: API Reference
nav_order: 4
description: "Complete API reference for all meta-log functions and modules"
permalink: /API-REFERENCE
---

# Complete API Reference

Comprehensive reference for all meta-log APIs, organized by component.

## Table of Contents

- [Core API](#core-api)
- [Prolog API](#prolog-api)
- [Datalog API](#datalog-api)
- [R5RS API](#r5rs-api)
- [M-Expression API](#m-expression-api)
- [Natural Language API](#natural-language-api)
- [Org Mode API](#org-mode-api)
- [MLSS API](#mlss-api)
- [Federation API](#federation-api)
- [Cryptography API](#cryptography-api)
- [Template API](#template-api)
- [E8 API](#e8-api)
- [p-Adic Arithmetic API](#p-adic-arithmetic-api)
- [Shimura Curves API](#shimura-curves-api)
- [Partition Analysis API](#partition-analysis-api)
- [Quaternion Algebra API](#quaternion-algebra-api)
- [Quadratic Forms API](#quadratic-forms-api)

---

## Core API

### Initialization

#### `meta-log-initialize`

Initialize the meta-log system.

**Signature**: `(meta-log-initialize &optional options)`

**Arguments**:
- `options` (plist, optional): Configuration options
  - `:r5rs-engine-path` (string): Path to R5RS engine
  - `:automata-path` (string): Path to automaton-evolutions
  - `:org-blackboard-path` (string): Path to Org blackboard file

**Returns**: `t` on success

**Example**:
```elisp
(meta-log-initialize '(:r5rs-engine-path "/path/to/r5rs-canvas-engine.scm"
                       :automata-path "/path/to/automaton-evolutions"))
```

---

## Prolog API

### Query Functions

#### `meta-log-prolog-query`

Execute a Prolog query.

**Signature**: `(meta-log-prolog-query goal)`

**Arguments**:
- `goal` (list): Prolog goal, e.g., `'(node ?Id ?Type)`

**Returns**: List of binding sets (alists)

**Example**:
```elisp
(meta-log-prolog-query '(node ?Id ?Type))
;; => (((?Id . node1) (?Type . text))
;;     ((?Id . node2) (?Type . code)))
```

#### `meta-log-prolog-add-fact`

Add a fact to Prolog database.

**Signature**: `(meta-log-prolog-add-fact predicate &rest args)`

**Example**:
```elisp
(meta-log-prolog-add-fact 'node 'node1 'text)
```

#### `meta-log-prolog-add-rule`

Add a rule to Prolog database.

**Signature**: `(meta-log-prolog-add-rule head &rest body)`

**Example**:
```elisp
(meta-log-prolog-add-rule '(inherits ?X ?Z)
                          '(vertical ?Y ?X)
                          '(inherits ?Y ?Z))
```

---

## Datalog API

### Query Functions

#### `meta-log-datalog-query`

Execute a Datalog query.

**Signature**: `(meta-log-datalog-query goal)`

**Arguments**:
- `goal` (list): Datalog goal

**Returns**: List of matching facts

**Example**:
```elisp
(meta-log-datalog-query '(ancestor ?X ?Y))
```

#### `meta-log-datalog-add-fact`

Add a fact to Datalog database.

**Signature**: `(meta-log-datalog-add-fact predicate &rest args)`

#### `meta-log-datalog-add-rule`

Add a rule to Datalog database.

**Signature**: `(meta-log-datalog-add-rule head &rest body)`

---

## R5RS API

### Evaluation Functions

#### `meta-log-r5rs-eval`

Execute R5RS Scheme expression.

**Signature**: `(meta-log-r5rs-eval expression)`

**Arguments**:
- `expression` (string or list): Scheme expression

**Returns**: Evaluation result

**Example**:
```elisp
(meta-log-r5rs-eval "(church-add 2 3)")
;; => 5
```

#### `meta-log-r5rs-load`

Load a Scheme file.

**Signature**: `(meta-log-r5rs-load file-path)`

#### `meta-log-r5rs-call`

Call an R5RS function.

**Signature**: `(meta-log-r5rs-call function-name &rest args)`

**Example**:
```elisp
(meta-log-r5rs-call "r5rs:church-add" 2 3)
```

#### `meta-log-r5rs-load-engine`

Load R5RS engine from file.

**Signature**: `(meta-log-r5rs-load-engine path)`

#### `meta-log-r5rs-eval-interactive`

**Status**: ⚠️ **Interactive Function** - Called by users via M-x, not programmatically

Evaluate R5RS Scheme expression interactively.

**Signature**: `(meta-log-r5rs-eval-interactive expression-string)`

**Arguments**:
- `expression-string` (string): Scheme expression as string

**Returns**: Evaluation result (formatted for display)

**Use Case**: Interactive evaluation of Scheme expressions in Emacs.

**Example**: Call via `M-x meta-log-r5rs-eval-interactive` and enter expression when prompted.

---

## M-Expression API

### Evaluation Functions

#### `meta-log-m-expr-eval`

Evaluate an M-expression.

**Signature**: `(meta-log-m-expr-eval m-expr-string)`

**Arguments**:
- `m-expr-string` (string): M-expression as string

**Returns**: Evaluation result

**Example**:
```elisp
(meta-log-m-expr-eval "eval[church-add[2; 3]; environment[global]]")
```

#### `meta-log-m-to-s`

Convert M-expression to S-expression.

**Signature**: `(meta-log-m-to-s m-expr)`

#### `meta-log-s-to-m`

Convert S-expression to M-expression string.

**Signature**: `(meta-log-s-to-m s-expr)`

---

## Natural Language API

### Query Functions

#### `meta-log-ask`

Ask a natural language question.

**Signature**: `(meta-log-ask question)`

**Arguments**:
- `question` (string): User's question in plain English

**Returns**: Human-readable answer string

**Example**:
```elisp
(meta-log-ask "What agents are in dimension 5D?")
```

---

## Org Mode API

### Blackboard Functions

#### `meta-log-org-load-blackboard`

Load an Org Mode blackboard file.

**Signature**: `(meta-log-org-load-blackboard file-path)`

#### `meta-log-org-save-blackboard`

Save current state to blackboard file.

**Signature**: `(meta-log-org-save-blackboard file-path)`

#### `meta-log-org-extract-template`

Extract template from Org heading.

**Signature**: `(meta-log-org-extract-template heading)`

#### `meta-log-org-extract-facts`

Extract facts from Org AST.

**Status**: ⚠️ **Partial** - Structure exists, implementation may be incomplete.

**Signature**: `(meta-log-org-extract-facts ast)`

---

## Automata API

### Loading Functions

#### `meta-log-load-automata`

Load automata from automaton-evolutions.

**Signature**: `(meta-log-load-automata path)`

**Arguments**:
- `path` (string): Path to automaton-evolutions package

#### `meta-log-find-evolutions-package`

Find automaton-evolutions installation.

**Signature**: `(meta-log-find-evolutions-package)`

**Returns**: Path to package or nil

---

## MLSS API

**Implementation Status Indicators**:
- ✅ **Complete** - Fully implemented and functional
- ⚠️ **Partial** - Core structure implemented, some functions have placeholder implementations
- ❌ **Placeholder** - Function exists but returns placeholder values

**Note**: Some MLSS APIs have placeholder implementations that need completion. See [AUDIT-REPORT.md](AUDIT-REPORT.md) and [API-VERIFICATION.md](API-VERIFICATION.md) for detailed status.

### Substrate Runtime

#### `substrate-create-memory`

Create a memory object in the substrate.

**Signature**: `(substrate-create-memory data metadata)`

**Arguments**:
- `data` (bytevector): Binary data
- `metadata` (alist): Metadata properties

**Returns**: Memory object with mlss:// URI

**Example**:
```scheme
(substrate-create-memory #u8(1 2 3 4) '((content-type . "test-data")))
```

#### `substrate-retrieve-memory`

Retrieve memory object by URI.

**Signature**: `(substrate-retrieve-memory uri)`

### Binary Layer (CBS)

#### `make-cbs`

Create a Canonical Binary Substrate object.

**Signature**: `(make-cbs bytes metadata)`

#### `binary-xor`

Apply XOR transformation to CBS.

**Signature**: `(binary-xor cbs mask)`

#### `binary-rotate`

Rotate CBS bytes.

**Signature**: `(binary-rotate cbs amount)`

### Waveform Layer

#### `make-waveform`

Create a waveform object.

**Signature**: `(make-waveform samples metadata sample-rate)`

#### `wdl-compile`

Compile Waveform Description Language to waveform.

**Status**: ⚠️ **Partial** - Core structure implemented, some waveform computation functions (FFT, p-adic, E8 signatures) have placeholder implementations.

**Signature**: `(wdl-compile wdl-expression)`

**Example**:
```scheme
(wdl-compile '(sine (freq 440) (amp 0.5) (duration 1.0)))
```

### Q* Optimality Engine

#### `make-qstar-state`

Create a Q* state object.

**Signature**: `(make-qstar-state variables goals)`

#### `qstar-evaluate`

Evaluate action in Q* state.

**Status**: ⚠️ **Partial** - Structure implemented, but scoring functions (qstar-score-euclidean, qstar-score-weyl, etc.) return placeholder values (0.0).

**Signature**: `(qstar-evaluate state action)`

**Returns**: `(value plan provenance)`

#### `qstar-select-action`

Select optimal action from action space.

**Status**: ⚠️ **Partial** - Structure implemented, depends on qstar-evaluate scoring functions.

**Signature**: `(qstar-select-action state action-space)`

### Computer Vision

#### `make-image`

Create an image object.

**Signature**: `(make-image width height channels pixels)`

#### `image-to-cbs`

Convert image to CBS format.

**Signature**: `(image-to-cbs image)`

#### `extract-features`

Extract features from image.

**Status**: ⚠️ **Partial** - Basic implementation exists, but FastAPI vision service functions (extract-sift, extract-orb, match-features-api) are placeholders that raise errors.

**Signature**: `(extract-features image)`

### Consciousness Framework

#### `make-conscious-state`

Create a consciousness state.

**Signature**: `(make-conscious-state action observation phase)`

#### `emerge-qualia`

Compute qualia emergence from state.

**Signature**: `(emerge-qualia action observation phase threshold curvature-factor)`

#### `collect-metrics`

Collect consciousness quality metrics.

**Signature**: `(collect-metrics current-state previous-state qualia-field)`

### Autonomy & Awareness

#### `autonomous-cycle`

Execute one autonomous cycle (perceive → decide → act → learn).

**Signature**: `(autonomous-cycle state sensors)`

#### `monitor-own-state`

Monitor system's own consciousness state.

**Signature**: `(monitor-own-state state)`

#### `reflect-on-action`

Reflect on past action outcome.

**Signature**: `(reflect-on-action action outcome)`

---

## Federation API

### Peer Management

#### `meta-log-federation-init`

Initialize federation system.

**Signature**: `(meta-log-federation-init blackboard-path)`

#### `meta-log-federation-connect`

Connect to a peer.

**Signature**: `(meta-log-federation-connect peer-id)`

#### `meta-log-federation-sync`

Synchronize with peers.

**Signature**: `(meta-log-federation-sync)`

---

## Cryptography API

### Identity Management

#### `meta-log-crypto-generate-identity`

Generate new cryptographic identity.

**Signature**: `(meta-log-crypto-generate-identity &optional mnemonic)`

#### `meta-log-crypto-derive-key`

Derive key from BIP32 path.

**Signature**: `(meta-log-crypto-derive-key path)`

---

## Template API

### Template Discovery

#### `meta-log-template-discover`

Discover templates matching query.

**Signature**: `(meta-log-template-discover query)`

#### `meta-log-template-federation-share`

Share template with peers.

**Signature**: `(meta-log-template-federation-share template-id)`

---

## E8 API

### Lattice Operations

#### `meta-log-e8-bip32-to-e8`

Map BIP32 path to E8 point.

**Signature**: `(meta-log-e8-bip32-to-e8 path)`

#### `meta-log-e8-distance`

Compute distance between E8 points.

**Signature**: `(meta-log-e8-distance point1 point2)`

#### `meta-log-e8-theta-series-create`

Create E8 theta series.

**Signature**: `(meta-log-e8-theta-series-create max-n)`

#### `meta-log-e8-verify-frbac-delegation`

Verify FRBAC delegation using E8 automorphisms.

**Signature**: `(meta-log-e8-verify-frbac-delegation master delegate)`

**Arguments**:
- `master` (E8Point): Master key point
- `delegate` (E8Point): Delegate key point

**Returns**: Boolean indicating if delegation is valid

#### `meta-log-e8-distance-for-ml`

Compute E8 distance metrics for ML features.

**Signature**: `(meta-log-e8-distance-for-ml point1 point2)`

**Returns**: Plist with :euclidean, :geodesic, :padic-2, :padic-3, :weyl-distance

#### `meta-log-e8-theta-coefficient`

Get r_E8(n): theta series coefficient.

**Signature**: `(meta-log-e8-theta-coefficient theta-series n)`

**Arguments**:
- `theta-series` (E8ThetaSeries): Theta series instance
- `n` (integer): Coefficient index

**Returns**: Integer coefficient value

**Example**:
```elisp
(require 'meta-log-e8-theta)
(let ((theta (meta-log-e8-theta-series-create 10)))
  (meta-log-e8-theta-coefficient theta 1))  ; Returns 240
```

#### `meta-log-e8-theta-predict-quorum-stability`

Predict election quorum stability using theta series.

**Signature**: `(meta-log-e8-theta-predict-quorum-stability theta-series voter-features)`

**Returns**: Plist with :stability-score, :qqf-determinant, :theta-growth, :form-type

---

## p-Adic Arithmetic API

### Basic Operations

#### `meta-log-p-adic-valuation`

Calculate p-adic valuation v_p(n) = ord_p(n).

**Signature**: `(meta-log-p-adic-valuation n p)`

**Arguments**:
- `n` (integer or rational): Number to evaluate
- `p` (prime): Prime number

**Returns**: Highest power of p dividing n (or negative for rationals)

**Example**:
```elisp
(meta-log-p-adic-valuation 12 2)  ; Returns 2 (2² divides 12)
(meta-log-p-adic-valuation 12 3)  ; Returns 1 (3¹ divides 12)
```

#### `meta-log-p-adic-norm`

Calculate p-adic norm |x|_p = p^{-v_p(x)}.

**Signature**: `(meta-log-p-adic-norm x p)`

**Arguments**:
- `x` (integer or rational): Number to evaluate
- `p` (prime): Prime number

**Returns**: p-adic norm (float)

**Example**:
```elisp
(meta-log-p-adic-norm 12 2)  ; Returns 0.25 (2^{-2})
```

### Advanced Features

#### `meta-log-p-adic-upper-half-plane-create`

**Status**: ⚠️ **Advanced Feature** - p-adic upper half-plane for advanced mathematical operations

Create p-adic upper half-plane Ω_p = ℂ_p \ ℚ_p.

**Signature**: `(meta-log-p-adic-upper-half-plane-create p)`

**Arguments**:
- `p` (prime): Prime number

**Returns**: meta-log-p-adic-upper-half-plane structure

**Use Case**: Advanced p-adic geometry and uniformization theory.

#### `meta-log-p-adic-point-in-hp`

**Status**: ⚠️ **Advanced Feature** - Point membership testing in p-adic upper half-plane

Check if point is in p-adic upper half-plane.

**Signature**: `(meta-log-p-adic-point-in-hp z p)`

**Arguments**:
- `z` (complex or p-adic): Point to test
- `p` (prime): Prime number

**Returns**: t if z ∈ Ω_p, nil otherwise

#### `meta-log-p-adic-modular-form`

**Status**: ⚠️ **Advanced Feature** - p-adic modular forms for number theory applications

Evaluate p-adic modular form.

**Signature**: `(meta-log-p-adic-modular-form form p weight)`

**Arguments**:
- `form` (structure): Modular form structure
- `p` (prime): Prime number
- `weight` (integer): Weight of the form

**Returns**: p-adic evaluation

### Specialized Features

#### `meta-log-p-adic-voter-features`

**Status**: ⚠️ **Specialized Feature** - Extract p-adic features from voter graphs for ML prediction

Extract p-adic features from voter graph for machine learning prediction.

**Signature**: `(meta-log-p-adic-voter-features voter-graph p)`

**Arguments**:
- `voter-graph` (structure): Graph structure with closeness features
- `p` (prime): Prime for p-adic valuation

**Returns**: Feature vector with p-adic valuations, Hilbert symbols, and ramification flags

**Use Case**: 
```elisp
;; Extract p-adic features from voter network for quorum stability prediction
(let ((voter-graph '((nodes . (n1 n2 n3 n4))
                     (edges . ((n1 . n2) (n2 . n3) (n3 . n4)))))
      (features (meta-log-p-adic-voter-features voter-graph 2)))
  ;; Features include: closeness values, p-adic valuations, Hilbert symbols, ramification flags
  features)
```

#### `meta-log-extract-closeness`

Extract closeness centrality features from graph.

**Signature**: `(meta-log-extract-closeness graph)`

**Arguments**:
- `graph` (structure): Graph structure (alist with nodes and edges)

**Returns**: List of closeness values for each node

**Example**:
```elisp
(let ((graph '((nodes . (a b c))
               (edges . ((a . b) (b . c)))))
      (closeness (meta-log-extract-closeness graph)))
  closeness)  ; Returns list of closeness centrality values
```

#### `meta-log-modular-form-coefficient`

Extract modular form coefficient.

**Signature**: `(meta-log-modular-form-coefficient form n)`

**Arguments**:
- `form` (structure): Modular form structure
- `n` (integer): Coefficient index

**Returns**: Coefficient value

---

## Shimura Curves API

**Status**: ⚠️ **Advanced Feature** - p-Adic Shimura curve uniformization via Cerednik-Drinfeld theorem

### Curve Creation

#### `meta-log-shimura-curve-create`

Create Shimura curve from quaternion algebra.

**Signature**: `(meta-log-shimura-curve-create algebra p)`

**Arguments**:
- `algebra` (meta-log-quaternion-algebra): Quaternion algebra (must be ramified at p)
- `p` (prime): Prime number

**Returns**: meta-log-shimura-curve structure or nil if algebra not ramified at p

**Use Case**: 
```elisp
;; Create Shimura curve for local-global consensus
(let ((algebra (meta-log-quaternion-algebra-create -1 -1))
      (curve (meta-log-shimura-curve-create algebra 2)))
  curve)
```

### Uniformization

#### `meta-log-shimura-p-adic-uniformize`

Uniformize Shimura curve via Cerednik-Drinfeld at prime p.

**Signature**: `(meta-log-shimura-p-adic-uniformize curve)`

**Arguments**:
- `curve` (meta-log-shimura-curve): Shimura curve structure

**Returns**: Rigid analytic structure for p-adic upper half-plane uniformization

**Use Case**: Bridge function fields to p-adic geometry for local-global consensus verification.

---

## Partition Analysis API

### Basic Operations

#### `meta-log-partition-calculate-betti-0`

Calculate Betti number β₀ (connected components).

**Signature**: `(meta-log-partition-calculate-betti-0 vertices edges)`

**Arguments**:
- `vertices` (list): List of vertex identifiers
- `edges` (list): List of (from . to) pairs

**Returns**: β₀ (number of connected components)

**Example**:
```elisp
(meta-log-partition-calculate-betti-0 '(a b c) '((a . b) (b . c)))
;; Returns 1 (single connected component)
```

### Specialized Features

#### `meta-log-partition-detect`

**Status**: ⚠️ **Specialized Feature** - Network partition detection via Betti numbers

Detect network partition via Betti number β₀.

**Signature**: `(meta-log-partition-detect vertices edges)`

**Arguments**:
- `vertices` (list): List of vertex identifiers
- `edges` (list): List of (from . to) pairs

**Returns**: Partition info alist with keys:
- `:is-partitioned` - boolean
- `:partition-count` - β₀ value
- `:components` - list of component vertex sets
- `:betti-0` - β₀ value

**Use Case**:
```elisp
;; Detect network partition in distributed system
(let ((vertices '(node1 node2 node3 node4))
      (edges '((node1 . node2) (node3 . node4))))  ; Two disconnected components
  (meta-log-partition-detect vertices edges))
;; Returns: ((:is-partitioned . t) (:partition-count . 2) ...)
```

#### `meta-log-partition-decompose`

**Status**: ⚠️ **Specialized Feature** - Geometric decomposition under partition

Decompose geometric type under partition.

**Signature**: `(meta-log-partition-decompose geometric-type partition-count)`

**Arguments**:
- `geometric-type` (symbol): Symbol (cube, octahedron, etc.)
- `partition-count` (integer): β₀ (number of partitions)

**Returns**: Decomposed type symbol

**Use Case**: Determine geometric structure after network partition.

#### `meta-log-partition-recover`

**Status**: ⚠️ **Specialized Feature** - Recover unified consensus from partition states

Recover unified consensus from partition states using duality.

**Signature**: `(meta-log-partition-recover partition-certs original-type)`

**Arguments**:
- `partition-certs` (list): List of consensus certificates from each partition
- `original-type` (symbol): Original geometric type (pre-partition)

**Returns**: Unified consensus certificate alist

**Use Case**:
```elisp
;; Recover consensus after network partition heals
(let ((certs '(((:agrees-count . 5) (:vertices . 10))
               ((:agrees-count . 4) (:vertices . 8))))
      (original 'cube))
  (meta-log-partition-recover certs original))
```

#### `meta-log-partition-padic-height-e8`

**Status**: ⚠️ **Specialized Feature** - E8 p-adic height for partition risk detection

Compute p-adic height on E8 lattice for partition detection.

**Signature**: `(meta-log-partition-padic-height-e8 vertex-point p)`

**Arguments**:
- `vertex-point` (meta-log-e8-point): E8 point structure representing a vertex
- `p` (prime): Prime number

**Returns**: Float height value (high values indicate ramification at p, suggesting partition risk)

**Use Case**: Detect partition risk by analyzing p-adic ramification on E8 lattice.

#### `meta-log-partition-detect-e8-ramification`

**Status**: ⚠️ **Specialized Feature** - Network partition detection via E8 p-adic ramification

Detect network partition via E8 p-adic ramification analysis.

**Signature**: `(meta-log-partition-detect-e8-ramification vertices edges &optional primes)`

**Arguments**:
- `vertices` (list): List of vertex identifiers
- `edges` (list): List of (from . to) pairs
- `primes` (list, optional): List of primes to check (default: [2, 3, 5, 7, 11])

**Returns**: Partition detection result alist with ramification analysis

**Use Case**:
```elisp
;; Detect partition risk using E8 p-adic ramification
(require 'meta-log-e8)
(let ((vertices '(v1 v2 v3 v4))
      (edges '((v1 . v2) (v3 . v4)))  ; Two disconnected components
      (result (meta-log-partition-detect-e8-ramification vertices edges '(2 3 5))))
  ;; Check ramification at each prime
  result)
```

---

## Quaternion Algebra API

### Basic Operations

#### `meta-log-quaternion-algebra-create`

Create quaternion algebra (a,b/F).

**Signature**: `(meta-log-quaternion-algebra-create a b &optional field)`

**Arguments**:
- `a` (number): Field element (typically integer for F=ℚ)
- `b` (number): Field element
- `field` (symbol, optional): Base field symbol (default: 'rational)

**Returns**: meta-log-quaternion-algebra structure

**Example**:
```elisp
(meta-log-quaternion-algebra-create -1 -1)  ; Standard quaternion algebra
```

#### `meta-log-quaternion-element-create`

Create quaternion element: t + xi + yj + zk.

**Signature**: `(meta-log-quaternion-element-create t-val x y z algebra)`

**Arguments**:
- `t-val`, `x`, `y`, `z` (numbers): Coefficients
- `algebra` (meta-log-quaternion-algebra): Quaternion algebra structure

**Returns**: meta-log-quaternion-element structure

#### `meta-log-quaternion-norm`

Calculate reduced norm Nrd(α) = α·ᾱ for quaternion element.

**Signature**: `(meta-log-quaternion-norm q)`

**Arguments**:
- `q` (meta-log-quaternion-element): Quaternion element

**Returns**: Norm value (norm form: t² - a x² - b y² + a b z²)

#### `meta-log-quaternion-norm-form`

Get norm form as quaternary quadratic form.

**Signature**: `(meta-log-quaternion-norm-form algebra)`

**Arguments**:
- `algebra` (meta-log-quaternion-algebra): Quaternion algebra

**Returns**: meta-log-qqf structure

### Advanced Features

#### `meta-log-quaternion-hilbert-symbol`

**Status**: ⚠️ **Advanced Feature** - Hilbert symbol for quaternion algebra splitting

Calculate Hilbert symbol (a,b)_p for quaternion algebra.

**Signature**: `(meta-log-quaternion-hilbert-symbol algebra p)`

**Arguments**:
- `algebra` (meta-log-quaternion-algebra): Quaternion algebra
- `p` (prime): Prime number

**Returns**: 1 if split at p, -1 if division algebra at p

#### `meta-log-hilbert-symbol`

**Status**: ⚠️ **Advanced Feature** - General Hilbert symbol computation

Calculate Hilbert symbol (a,b)_p.

**Signature**: `(meta-log-hilbert-symbol a b p)`

**Arguments**:
- `a`, `b` (integers): Integers
- `p` (prime): Prime

**Returns**: 1 if ax² + by² = z² has solution in ℚ_p, -1 otherwise

#### `meta-log-hilbert-symbol-2`

**Status**: ⚠️ **Advanced Feature** - Hilbert symbol for p=2

Hilbert symbol for p=2.

**Signature**: `(meta-log-hilbert-symbol-2 a b)`

**Arguments**:
- `a`, `b` (integers): Integers

**Returns**: 1 or -1

#### `meta-log-hilbert-symbol-reduce`

**Status**: ⚠️ **Advanced Feature** - Reduced Hilbert symbol computation

Reduce Hilbert symbol computation when valuations are non-zero.

**Signature**: `(meta-log-hilbert-symbol-reduce a b p va vb)`

**Arguments**:
- `a`, `b` (integers): Integers
- `p` (prime): Prime
- `va`, `vb` (integers): p-adic valuations

**Returns**: Reduced Hilbert symbol value

#### `meta-log-legendre-symbol`

**Status**: ⚠️ **Advanced Feature** - Legendre symbol computation

Calculate Legendre symbol (a/p).

**Signature**: `(meta-log-legendre-symbol a p)`

**Arguments**:
- `a` (integer): Integer
- `p` (prime): Odd prime

**Returns**: 1, -1, or 0

#### `meta-log-quadratic-residue-p`

**Status**: ⚠️ **Advanced Feature** - Quadratic residue testing

Check if a is a quadratic residue mod p.

**Signature**: `(meta-log-quadratic-residue-p a p)`

**Arguments**:
- `a` (integer): Integer
- `p` (prime): Prime

**Returns**: t if a ≡ x² (mod p) has solution, nil otherwise

#### `meta-log-quaternion-discriminant`

**Status**: ⚠️ **Advanced Feature** - Discriminant of quaternion algebra

Calculate discriminant of quaternion algebra (product of ramified primes).

**Signature**: `(meta-log-quaternion-discriminant algebra)`

**Arguments**:
- `algebra` (meta-log-quaternion-algebra): Quaternion algebra

**Returns**: List of ramified primes

### Specialized Features

#### `meta-log-quaternion-bip32-path`

**Status**: ⚠️ **Specialized Feature** - Map BIP32 derivation path to quaternion basis

Map BIP32 derivation path to quaternion basis.

**Signature**: `(meta-log-quaternion-bip32-path algebra path)`

**Arguments**:
- `algebra` (meta-log-quaternion-algebra): Quaternion algebra
- `path` (string): BIP32 path string (e.g., "m/44'/0'/0'/0/0")

**Returns**: Quaternion element corresponding to path

**Use Case**:
```elisp
;; Map BIP32 path to quaternion element for cryptographic key derivation
(let ((algebra (meta-log-quaternion-algebra-create -1 -1))
      (path "m/44'/0'/0'/0/0"))
  (meta-log-quaternion-bip32-path algebra path))
```

---

## Quadratic Forms API

### Binary Quadratic Forms (BQF)

#### `meta-log-bqf-create`

Create a binary quadratic form: ax² + bxy + cy².

**Signature**: `(meta-log-bqf-create a b c)`

**Arguments**:
- `a`, `b`, `c` (numbers): Coefficients

**Returns**: meta-log-bqf structure

**Example**:
```elisp
(meta-log-bqf-create 1 0 1)  ; x² + y²
```

#### `meta-log-bqf-discriminant`

Calculate discriminant Δ = b² - 4ac for BQF.

**Signature**: `(meta-log-bqf-discriminant bqf)`

**Arguments**:
- `bqf` (meta-log-bqf): BQF structure

**Returns**: Discriminant value

#### `meta-log-bqf-classify`

Classify BQF by discriminant.

**Signature**: `(meta-log-bqf-classify bqf)`

**Arguments**:
- `bqf` (meta-log-bqf): BQF structure

**Returns**: Symbol: 'positive-definite, 'negative-definite, 'indefinite, or 'degenerate

#### `meta-log-bqf-from-coefficients`

Create BQF from coefficient list [a, b, c].

**Signature**: `(meta-log-bqf-from-coefficients coefficients)`

**Arguments**:
- `coefficients` (list): List of 3 numbers

**Returns**: meta-log-bqf structure

#### `meta-log-bqf-from-canvasl`

Extract BQF from CanvasL bipartite.bqf object.

**Signature**: `(meta-log-bqf-from-canvasl bqf-obj)`

**Arguments**:
- `bqf-obj` (structure): Parsed JSON object with 'coefficients and 'form fields

**Returns**: meta-log-bqf structure or nil

### Ternary Quadratic Forms (TQF)

#### `meta-log-tqf-create`

Create a ternary quadratic form: ax² + by² + cz² + 2dxy + 2exz + 2fyz.

**Signature**: `(meta-log-tqf-create a b c d e f)`

**Arguments**:
- `a`, `b`, `c` (numbers): Diagonal coefficients
- `d`, `e`, `f` (numbers): Cross-term coefficients (halved)

**Returns**: meta-log-tqf structure

#### `meta-log-tqf-discriminant`

Calculate discriminant Δ for TQF.

**Signature**: `(meta-log-tqf-discriminant tqf)`

**Arguments**:
- `tqf` (meta-log-tqf): TQF structure

**Returns**: Discriminant value (formula: Δ = a*b*c - a*f² - b*e² - c*d² + 2*d*e*f)

#### `meta-log-tqf-classify`

Classify TQF by discriminant.

**Signature**: `(meta-log-tqf-classify tqf)`

**Arguments**:
- `tqf` (meta-log-tqf): TQF structure

**Returns**: Symbol: 'positive-definite, 'indefinite, or 'degenerate

### Quaternary Quadratic Forms (QQF)

#### `meta-log-qqf-create`

Create a quaternary quadratic form.

**Signature**: `(meta-log-qqf-create a b c d e f g h i j)`

**Arguments**:
- `a` through `j` (numbers): 10 coefficients

**Returns**: meta-log-qqf structure

#### `meta-log-qqf-discriminant`

Calculate discriminant for QQF.

**Signature**: `(meta-log-qqf-discriminant qqf)`

**Arguments**:
- `qqf` (meta-log-qqf): QQF structure

**Returns**: Discriminant value (4x4 matrix determinant)

#### `meta-log-qqf-classify`

Classify QQF by discriminant.

**Signature**: `(meta-log-qqf-classify qqf)`

**Arguments**:
- `qqf` (meta-log-qqf): QQF structure

**Returns**: Classification symbol

### Matrix Operations

#### `meta-log-matrix-determinant-4x4`

**Status**: ⚠️ **Advanced Feature** - 4x4 matrix determinant for QQF discriminant

Calculate determinant of 4x4 matrix.

**Signature**: `(meta-log-matrix-determinant-4x4 matrix)`

**Arguments**:
- `matrix` (list or vector): 4x4 matrix

**Returns**: Determinant value

#### `meta-log-matrix-determinant-3x3`

**Status**: ⚠️ **Advanced Feature** - 3x3 matrix determinant

Calculate determinant of 3x3 matrix.

**Signature**: `(meta-log-matrix-determinant-3x3 matrix)`

**Arguments**:
- `matrix` (list or vector): 3x3 matrix

**Returns**: Determinant value

### Specialized Features

#### `meta-log-quadratic-forms-classify-consensus`

**Status**: ⚠️ **Specialized Feature** - Consensus classification using multiple quadratic forms

Classify consensus stability using quadratic form discriminant.

**Signature**: `(meta-log-quadratic-forms-classify-consensus form-type form-data)`

**Arguments**:
- `form-type` (symbol): 'bqf, 'tqf, or 'qqf
- `form-data` (structure): Form structure or CanvasL data

**Returns**: Classification result alist with keys:
- `:classification` - Classification symbol
- `:stable` - Boolean (true for positive/negative definite)
- `:indefinite` - Boolean
- `:degenerate` - Boolean

**Use Case**:
```elisp
;; Classify consensus stability using binary quadratic form
(let ((bqf (meta-log-bqf-create 1 0 1))  ; x² + y² (positive definite)
      (result (meta-log-quadratic-forms-classify-consensus 'bqf bqf)))
  (plist-get result :stable))  ; Returns t (stable consensus)

;; Classify using quaternary quadratic form
(let ((qqf (meta-log-qqf-create 1 1 1 1 0 0 0 0 0 0))
      (result (meta-log-quadratic-forms-classify-consensus 'qqf qqf)))
  result)
```

#### `meta-log-qqf-e8-theta-link`

**Status**: ⚠️ **Specialized Feature** - Link QQF to E8 theta series for quorum stability prediction

Link quaternary quadratic form to E8 theta series for stability analysis.

**Signature**: `(meta-log-qqf-e8-theta-link qqf &optional theta-series)`

**Arguments**:
- `qqf` (meta-log-qqf): QQF structure
- `theta-series` (meta-log-e8-theta-series, optional): E8 theta series (created if not provided)

**Returns**: Link structure with stability metrics including:
- `:qqf-determinant` - QQF discriminant value
- `:theta-coefficients` - E8 theta series coefficients
- `:stability-indicator` - Stability prediction

**Use Case**:
```elisp
;; Link QQF to E8 theta series for quorum stability prediction
(require 'meta-log-e8-theta)
(let ((qqf (meta-log-qqf-create 1 1 1 1 0 0 0 0 0 0))
      (theta (meta-log-e8-theta-series-create 10))
      (link (meta-log-qqf-e8-theta-link qqf theta)))
  ;; Use link for stability prediction
  (plist-get link :stability-indicator))
```

---

## FastAPI Services

### E8 API Service

**Base URL**: `http://localhost:8000/api/v1`

- `GET /health` - Health check
- `POST /e8/bip32-to-point` - Map BIP32 to E8
- `POST /e8/distance` - Compute distance
- `POST /e8/shortest-path` - Find shortest path

### Substrate API Service

**Base URL**: `http://localhost:8001/api/v1`

- `GET /health` - Health check
- `POST /substrate/create` - Create memory object
- `GET /substrate/{uri}` - Retrieve memory object

### Vision API Service

**Base URL**: `http://localhost:8002/api/v1`

- `GET /health` - Health check
- `POST /vision/process` - Process image
- `POST /vision/features` - Extract features

### Sensors API Service

**Base URL**: `http://localhost:8003/api/v1`

- `GET /health` - Health check
- `GET /sensors/gps` - Get GPS position
- `GET /sensors/wifi` - Scan WiFi networks
- `GET /sensors/motion` - Read motion sensors

---

## See Also

- [User Guide](USER_GUIDE) - Usage examples
- [MLSS Guide](MLSS_GUIDE) - MLSS-specific APIs
- [Modules](MODULES) - Module-specific APIs
- [Glossary](GLOSSARY) - Term definitions

