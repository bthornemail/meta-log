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

**Signature**: `(qstar-evaluate state action)`

**Returns**: `(value plan provenance)`

#### `qstar-select-action`

Select optimal action from action space.

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

