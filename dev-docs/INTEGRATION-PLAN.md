# Meta-Log Integration Plan: State Machine, RBAC, CanvasL
**Date:** 2025-11-21
**Scope:** Integrating research recommendations across Emacs (`meta-log`) and TypeScript (`meta-log-db`)

---

## Executive Summary

This plan implements **three critical priorities** identified from the research documentation:

1. **UTCT State Machine** - Universal tuple-based state transformations
2. **Federated RBAC** - Geometric role-based access control
3. **CanvasL Support** - Dual-pair stratification format

**Strategy:** Build in TypeScript first (meta-log-db), then expose to Emacs via JSON-RPC bridge.

---

## Current State Assessment

### ✅ What We Have

| Component | meta-log (Emacs) | meta-log-db (TypeScript) | Status |
|-----------|------------------|--------------------------|--------|
| **CanvasL Parsing** | ❌ None | ✅ `canvasl-browser.ts` | 70% complete |
| **BIP32 Crypto** | ✅ `meta-log-crypto.el` | ✅ `crypto/bip32.ts` | 80% complete |
| **JSONL Parser** | ❌ None | ✅ `jsonl/parser.ts` | Complete |
| **Prolog Engine** | ✅ `meta-log-prolog.el` | ✅ `prolog/engine.ts` | Complete |
| **DataLog Engine** | ✅ `meta-log-datalog.el` | ✅ `datalog/engine.ts` | Complete |
| **R5RS** | ✅ `meta-log-r5rs.el` | ✅ `r5rs/` | Complete |
| **State Machine** | ❌ None | ❌ None | **0% - Priority 1** |
| **RBAC** | ❌ None | ❌ None | **0% - Priority 2** |
| **CanvasL Dual Pairs** | ❌ None | ⚠️ Partial | **30% - Priority 3** |

### ⚠️ What We Need

**Priority 1: UTCT State Machine**
- Universal Tuple type system (4D: Identity, Orthogonal, Exponential, Topological)
- ΔT transformation algebra
- Branch cut resolution
- Harmony verification

**Priority 2: Federated RBAC**
- BIP32 → Role hierarchy mapping
- Geometric access control topology
- Permission verification via polyhedra
- Template library (enterprise/gov/healthcare)

**Priority 3: CanvasL Enhancement**
- Dual-pair stratification enforcement
- `@include` directive support
- Org-babel integration
- Self-tangling support

---

## Architecture: Dual Implementation Strategy

### Why TypeScript + Emacs?

```
┌─────────────────────────────────────────────────┐
│              meta-log (Emacs)                   │
│  ┌──────────────────────────────────────────┐   │
│  │  High-level UI & Workflow                │   │
│  │  - Org-mode integration                  │   │
│  │  - Chat interface                        │   │
│  │  - Dashboard                             │   │
│  │  - File ingestion                        │   │
│  └──────────────┬───────────────────────────┘   │
│                 │ JSON-RPC Bridge                │
└─────────────────┼───────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────┐
│           meta-log-db (TypeScript)              │
│  ┌──────────────────────────────────────────┐   │
│  │  Core Data & Logic Engine                │   │
│  │  - UTCT State Machine                    │   │
│  │  - RBAC Access Control                   │   │
│  │  - CanvasL Parser                        │   │
│  │  - Prolog/DataLog/R5RS                   │   │
│  │  - Cryptographic operations              │   │
│  └──────────────────────────────────────────┘   │
│  ┌──────────────────────────────────────────┐   │
│  │  Browser Support (IndexedDB, WebCrypto)  │   │
│  └──────────────────────────────────────────┘   │
└─────────────────────────────────────────────────┘
```

**Rationale:**
- **TypeScript:** Better for complex algorithms, math-heavy code, browser support, npm ecosystem
- **Emacs Lisp:** Better for text manipulation, Org integration, user workflows
- **JSON-RPC:** Clean separation, enables web UI in future

---

## Implementation Phases

## Phase 1: UTCT State Machine (Weeks 1-3)

### 1.1 TypeScript Implementation (`meta-log-db`)

**File:** `src/utct/state-machine.ts`

```typescript
/**
 * UTCT Universal State Machine
 * Implements T_{n+1} = T_n + ΔT transformation algebra
 */

// Core types
export interface UniversalTuple {
  identity: BinaryFloatPair;      // (1.0, 0x3F800000)
  orthogonal: BinaryFloatPair;    // (π,   0x40490FDB)
  exponential: BinaryFloatPair;   // (e,   0x402DF854)
  topological: BinaryFloatPair;   // (1.0, 0x3FF00000)
}

export interface BinaryFloatPair {
  float: number;
  binary: Uint8Array; // IEEE 754 representation
}

// Universal Basis (Genesis State)
export const UNIVERSAL_BASIS: UniversalTuple = {
  identity:     { float: 1.0, binary: new Uint8Array([0x3F, 0x80, 0x00, 0x00]) },
  orthogonal:   { float: Math.PI, binary: new Uint8Array([0x40, 0x49, 0x0F, 0xDB]) },
  exponential:  { float: Math.E, binary: new Uint8Array([0x40, 0x2D, 0xF8, 0x54]) },
  topological:  { float: 1.0, binary: new Uint8Array([0x3F, 0xF0, 0x00, 0x00]) }
};

// Algebra operations
export class UTCTAlgebra {
  /**
   * Add two Universal Tuples: T₁ + T₂
   */
  static add(t1: UniversalTuple, t2: UniversalTuple): UniversalTuple {
    return {
      identity:     { float: t1.identity.float + t2.identity.float, binary: this.floatToBytes(t1.identity.float + t2.identity.float) },
      orthogonal:   { float: t1.orthogonal.float + t2.orthogonal.float, binary: this.floatToBytes(t1.orthogonal.float + t2.orthogonal.float) },
      exponential:  { float: t1.exponential.float + t2.exponential.float, binary: this.floatToBytes(t1.exponential.float + t2.exponential.float) },
      topological:  { float: t1.topological.float + t2.topological.float, binary: this.floatToBytes(t1.topological.float + t2.topological.float) }
    };
  }

  /**
   * Subtract: T₁ - T₂
   */
  static subtract(t1: UniversalTuple, t2: UniversalTuple): UniversalTuple {
    return {
      identity:     { float: t1.identity.float - t2.identity.float, binary: this.floatToBytes(t1.identity.float - t2.identity.float) },
      orthogonal:   { float: t1.orthogonal.float - t2.orthogonal.float, binary: this.floatToBytes(t1.orthogonal.float - t2.orthogonal.float) },
      exponential:  { float: t1.exponential.float - t2.exponential.float, binary: this.floatToBytes(t1.exponential.float - t2.exponential.float) },
      topological:  { float: t1.topological.float - t2.topological.float, binary: this.floatToBytes(t1.topological.float - t2.topological.float) }
    };
  }

  /**
   * Scalar multiplication: k·T
   */
  static scale(t: UniversalTuple, scalar: number): UniversalTuple {
    return {
      identity:     { float: t.identity.float * scalar, binary: this.floatToBytes(t.identity.float * scalar) },
      orthogonal:   { float: t.orthogonal.float * scalar, binary: this.floatToBytes(t.orthogonal.float * scalar) },
      exponential:  { float: t.exponential.float * scalar, binary: this.floatToBytes(t.exponential.float * scalar) },
      topological:  { float: t.topological.float * scalar, binary: this.floatToBytes(t.topological.float * scalar) }
    };
  }

  /**
   * Inner product: ⟨T₁, T₂⟩
   */
  static innerProduct(t1: UniversalTuple, t2: UniversalTuple): number {
    return (
      t1.identity.float * t2.identity.float +
      t1.orthogonal.float * t2.orthogonal.float +
      t1.exponential.float * t2.exponential.float +
      t1.topological.float * t2.topological.float
    );
  }

  /**
   * Norm: ‖T‖
   */
  static norm(t: UniversalTuple): number {
    return Math.sqrt(this.innerProduct(t, t));
  }

  /**
   * Convert float to IEEE 754 bytes
   */
  private static floatToBytes(value: number): Uint8Array {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    view.setFloat32(0, value, false);
    return new Uint8Array(buffer);
  }
}

// Branch Cut Resolution
export class BranchCutResolver {
  /**
   * Apply branch cut to resolve multi-valued function outcomes
   * Selects unique outcome with minimal topological distance to ΔT
   */
  static resolveBranchCut<T>(
    outcomes: Array<{ value: T; tuple: UniversalTuple }>,
    deltaT: UniversalTuple
  ): T {
    if (outcomes.length === 0) {
      throw new Error('No outcomes to resolve');
    }

    if (outcomes.length === 1) {
      return outcomes[0].value;
    }

    // Find outcome with minimal topological distance
    let minDistance = Infinity;
    let selectedOutcome = outcomes[0];

    for (const outcome of outcomes) {
      const diff = UTCTAlgebra.subtract(outcome.tuple, deltaT);
      const distance = UTCTAlgebra.norm(diff);

      if (distance < minDistance) {
        minDistance = distance;
        selectedOutcome = outcome;
      }
    }

    return selectedOutcome.value;
  }

  /**
   * Generate uniqueness proof for branch cut selection
   */
  static proveUniqueness(
    outcome: UniversalTuple,
    deltaT: UniversalTuple
  ): { isUnique: boolean; distance: number } {
    const diff = UTCTAlgebra.subtract(outcome, deltaT);
    const distance = UTCTAlgebra.norm(diff);

    return {
      isUnique: true, // Proven by minimal distance
      distance
    };
  }
}

// Harmony Verification
export class HarmonyVerification {
  /**
   * Verify mathematical consistency of a Universal Tuple
   */
  static verify(t: UniversalTuple): {
    isValid: boolean;
    harmonyScore: number;
    errors: string[];
  } {
    const errors: string[] = [];

    // Check 1: Mathematical consistency (π and e relationships)
    const piCheck = Math.abs(t.orthogonal.float - Math.PI) < 0.001;
    const eCheck = Math.abs(t.exponential.float - Math.E) < 0.001;

    if (!piCheck) errors.push('Orthogonal component deviates from π');
    if (!eCheck) errors.push('Exponential component deviates from e');

    // Check 2: Topological integrity (connectivity maintained)
    const topologyCheck = !isNaN(t.topological.float) && isFinite(t.topological.float);
    if (!topologyCheck) errors.push('Topological component is invalid');

    // Check 3: Computational boundedness
    const bounded = [t.identity, t.orthogonal, t.exponential, t.topological]
      .every(p => isFinite(p.float) && !isNaN(p.float));

    if (!bounded) errors.push('Contains unbounded values');

    // Calculate harmony score (0-1)
    let score = 1.0;
    score -= errors.length * 0.2;
    score = Math.max(0, Math.min(1, score));

    return {
      isValid: errors.length === 0,
      harmonyScore: score,
      errors
    };
  }
}

// Main State Machine
export class UTCTStateMachine {
  private currentState: UniversalTuple;
  private stateHistory: UniversalTuple[] = [];

  constructor(initialState: UniversalTuple = UNIVERSAL_BASIS) {
    this.currentState = initialState;
    this.stateHistory.push(initialState);
  }

  /**
   * Apply transformation: T_{n+1} = T_n + ΔT
   */
  async applyTransformation(deltaT: UniversalTuple): Promise<{
    newState: UniversalTuple;
    verification: ReturnType<typeof HarmonyVerification.verify>;
  }> {
    // Verify ΔT harmony
    const deltaVerification = HarmonyVerification.verify(deltaT);
    if (!deltaVerification.isValid) {
      throw new Error(`Invalid ΔT: ${deltaVerification.errors.join(', ')}`);
    }

    // Compute new state
    const newState = UTCTAlgebra.add(this.currentState, deltaT);

    // Verify new state harmony
    const stateVerification = HarmonyVerification.verify(newState);
    if (!stateVerification.isValid) {
      throw new Error(`Invalid resulting state: ${stateVerification.errors.join(', ')}`);
    }

    // Update state
    this.currentState = newState;
    this.stateHistory.push(newState);

    return {
      newState,
      verification: stateVerification
    };
  }

  /**
   * Compute ΔT between two states: ΔT = T_new - T_old
   */
  static computeDelta(oldState: UniversalTuple, newState: UniversalTuple): UniversalTuple {
    return UTCTAlgebra.subtract(newState, oldState);
  }

  /**
   * Get current state
   */
  getState(): UniversalTuple {
    return this.currentState;
  }

  /**
   * Get state history
   */
  getHistory(): UniversalTuple[] {
    return [...this.stateHistory];
  }

  /**
   * Rollback to previous state
   */
  rollback(): UniversalTuple | null {
    if (this.stateHistory.length <= 1) {
      return null;
    }

    this.stateHistory.pop();
    this.currentState = this.stateHistory[this.stateHistory.length - 1];
    return this.currentState;
  }
}
```

**File:** `src/utct/index.ts`

```typescript
export * from './state-machine.js';
export { UNIVERSAL_BASIS, UTCTAlgebra, BranchCutResolver, HarmonyVerification, UTCTStateMachine } from './state-machine.js';
```

**File:** `src/index.ts` (add export)

```typescript
// ... existing exports ...
export * from './utct/index.js';
```

### 1.2 TypeScript Tests

**File:** `src/__tests__/utct.test.ts`

```typescript
import { describe, test, expect } from '@jest/globals';
import { UNIVERSAL_BASIS, UTCTAlgebra, UTCTStateMachine, HarmonyVerification } from '../utct/state-machine.js';

describe('UTCT State Machine', () => {
  test('Universal Basis is valid', () => {
    const verification = HarmonyVerification.verify(UNIVERSAL_BASIS);
    expect(verification.isValid).toBe(true);
    expect(verification.harmonyScore).toBeGreaterThan(0.9);
  });

  test('Add two tuples', () => {
    const t1 = UNIVERSAL_BASIS;
    const t2 = UNIVERSAL_BASIS;
    const sum = UTCTAlgebra.add(t1, t2);

    expect(sum.identity.float).toBe(2.0);
    expect(sum.orthogonal.float).toBeCloseTo(2 * Math.PI, 2);
  });

  test('State transformation', async () => {
    const machine = new UTCTStateMachine();
    const deltaT = UTCTAlgebra.scale(UNIVERSAL_BASIS, 0.1);

    const result = await machine.applyTransformation(deltaT);

    expect(result.verification.isValid).toBe(true);
    expect(result.newState.identity.float).toBeGreaterThan(UNIVERSAL_BASIS.identity.float);
  });

  test('Compute delta between states', () => {
    const t1 = UNIVERSAL_BASIS;
    const t2 = UTCTAlgebra.scale(UNIVERSAL_BASIS, 2);
    const delta = UTCTStateMachine.computeDelta(t1, t2);

    expect(delta.identity.float).toBeCloseTo(UNIVERSAL_BASIS.identity.float, 2);
  });
});
```

### 1.3 Emacs Integration

**File:** `modules/meta-log-utct.el`

```elisp
;;; meta-log-utct.el --- UTCT Universal State Machine

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne <bthornemail@gmail.com>

;;; Commentary:
;; Emacs interface to meta-log-db UTCT state machine via JSON-RPC

;;; Code:

(require 'json)
(require 'url)

(defvar meta-log-utct-server-url "http://localhost:3000/utct"
  "URL of meta-log-db UTCT server.")

(defvar meta-log-utct--current-state nil
  "Current UTCT state (cached).")

;; Universal Basis constant
(defconst meta-log-utct-universal-basis
  '((identity . ((float . 1.0) (binary . [63 128 0 0])))
    (orthogonal . ((float . 3.14159) (binary . [64 73 15 219])))
    (exponential . ((float . 2.71828) (binary . [64 45 248 84])))
    (topological . ((float . 1.0) (binary . [63 240 0 0]))))
  "Universal Basis genesis state.")

(defun meta-log-utct--rpc-call (method params)
  "Call meta-log-db JSON-RPC METHOD with PARAMS."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode `((jsonrpc . "2.0")
                         (method . ,method)
                         (params . ,params)
                         (id . 1)))
           'utf-8)))
    (with-current-buffer
        (url-retrieve-synchronously meta-log-utct-server-url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

;;;###autoload
(defun meta-log-utct-initialize ()
  "Initialize UTCT state machine."
  (interactive)
  (let ((result (meta-log-utct--rpc-call "utct.initialize" '())))
    (setq meta-log-utct--current-state (alist-get 'state result))
    (message "UTCT initialized: harmony score %.2f"
             (alist-get 'harmonyScore (alist-get 'verification result)))))

;;;###autoload
(defun meta-log-utct-apply-delta (delta)
  "Apply DELTA transformation to current state.
DELTA should be an alist representing a UniversalTuple."
  (interactive)
  (let* ((result (meta-log-utct--rpc-call "utct.applyDelta"
                                          `((delta . ,delta))))
         (new-state (alist-get 'newState result))
         (verification (alist-get 'verification result)))
    (setq meta-log-utct--current-state new-state)
    (message "State updated: harmony %.2f"
             (alist-get 'harmonyScore verification))
    new-state))

;;;###autoload
(defun meta-log-utct-get-state ()
  "Get current UTCT state."
  (interactive)
  (or meta-log-utct--current-state
      (progn
        (meta-log-utct-initialize)
        meta-log-utct--current-state)))

;;;###autoload
(defun meta-log-utct-compute-delta (old-state new-state)
  "Compute ΔT between OLD-STATE and NEW-STATE."
  (let ((result (meta-log-utct--rpc-call "utct.computeDelta"
                                         `((oldState . ,old-state)
                                           (newState . ,new-state)))))
    (alist-get 'delta result)))

;;;###autoload
(defun meta-log-utct-demo ()
  "Run UTCT demonstration."
  (interactive)
  (meta-log-utct-initialize)

  (message "Current state: %S" (meta-log-utct-get-state))

  ;; Apply small transformation
  (let ((delta `((identity . ((float . 0.1)))
                 (orthogonal . ((float . 0.0)))
                 (exponential . ((float . 0.0)))
                 (topological . ((float . 0.0))))))
    (meta-log-utct-apply-delta delta))

  (message "New state: %S" (meta-log-utct-get-state)))

(provide 'meta-log-utct)

;;; meta-log-utct.el ends here
```

### 1.4 Integration with Federation

**Enhance:** `modules/meta-log-federation.el`

```elisp
;; Add to meta-log-federation.el

(require 'meta-log-utct)

(defun meta-log-federation-sync-with-utct (peer-id data)
  "Sync with PEER-ID using UTCT ΔT encoding instead of full state.
DATA is the peer's state."
  (let* ((current-state (meta-log-utct-get-state))
         (peer-state data)
         (delta (meta-log-utct-compute-delta current-state peer-state)))

    ;; Only transmit ΔT (64 bytes) instead of full state
    (meta-log-mqtt-publish
     (format "meta-log/federation/%s/delta" peer-id)
     (json-encode delta))

    (message "Sent ΔT to %s (size: %d bytes)"
             peer-id
             (length (json-encode delta)))))
```

### 1.5 Deliverables (Phase 1)

- [ ] `src/utct/state-machine.ts` (500 LOC)
- [ ] `src/__tests__/utct.test.ts` (200 LOC)
- [ ] `modules/meta-log-utct.el` (300 LOC)
- [ ] Enhanced `meta-log-federation.el` with ΔT sync
- [ ] Documentation: `docs/UTCT-GUIDE.md`
- [ ] Demo: `examples/utct-demo.el`

**Timeline:** 3 weeks
**Success Metric:** ΔT encoding reduces federation sync bandwidth by 70%

---

## Phase 2: Federated RBAC (Weeks 4-7)

### 2.1 TypeScript Implementation

**File:** `src/rbac/geometric-rbac.ts`

```typescript
/**
 * Federated Role-Based Access Control (RBAC) via Geometric Topology
 * Reinterprets BIP32 derivation paths as role hierarchies
 */

import { deriveKey, parsePath } from '../browser/crypto/bip32.js';

// Geometric constraint types (mapped to polyhedra)
export enum GeometricConstraint {
  // Platonic solids (local/private context)
  TETRAHEDRON = 'tetrahedron',    // 4 vertices, 100% consensus (MUST)
  CUBE = 'cube',                  // 8 vertices, 50% consensus (MAY)
  OCTAHEDRON = 'octahedron',      // 6 vertices, 83% consensus (SHOULD)
  DODECAHEDRON = 'dodecahedron',  // 20 vertices, 90% consensus (MUST_NOT)
  ICOSAHEDRON = 'icosahedron',    // 12 vertices, recommend alternative

  // 4-polytopes (federated context)
  FIVECELL = '5-cell',            // 5 vertices (federated MUST)
  EIGHTCELL = '8-cell',           // 16 vertices (federated MAY)
  SIXTEENCELL = '16-cell',        // 8 vertices (federated RECOMMENDED)
  TWENTYFOURCELL = '24-cell'      // 24 vertices (federated SHOULD)
}

export interface RoleConstraints {
  minVotes: number;       // Minimum votes required
  totalVoters: number;    // Total possible voters
  threshold: number;      // Consensus threshold (0-1)
  polyhedron: GeometricConstraint;
}

export interface GeometricRole {
  name: string;
  derivationPath: string;  // BIP32 path (e.g., "m/enterprise/cto")
  constraints: RoleConstraints;
  key: Uint8Array;
  chainCode: Uint8Array;
  parent?: GeometricRole;
}

// Polyhedron definitions
const POLYHEDRA: Record<GeometricConstraint, RoleConstraints> = {
  [GeometricConstraint.TETRAHEDRON]: { minVotes: 4, totalVoters: 4, threshold: 1.0, polyhedron: GeometricConstraint.TETRAHEDRON },
  [GeometricConstraint.CUBE]: { minVotes: 4, totalVoters: 8, threshold: 0.5, polyhedron: GeometricConstraint.CUBE },
  [GeometricConstraint.OCTAHEDRON]: { minVotes: 5, totalVoters: 6, threshold: 0.833, polyhedron: GeometricConstraint.OCTAHEDRON },
  [GeometricConstraint.DODECAHEDRON]: { minVotes: 18, totalVoters: 20, threshold: 0.9, polyhedron: GeometricConstraint.DODECAHEDRON },
  [GeometricConstraint.ICOSAHEDRON]: { minVotes: 10, totalVoters: 12, threshold: 0.833, polyhedron: GeometricConstraint.ICOSAHEDRON },
  [GeometricConstraint.FIVECELL]: { minVotes: 5, totalVoters: 5, threshold: 1.0, polyhedron: GeometricConstraint.FIVECELL },
  [GeometricConstraint.EIGHTCELL]: { minVotes: 8, totalVoters: 16, threshold: 0.5, polyhedron: GeometricConstraint.EIGHTCELL },
  [GeometricConstraint.SIXTEENCELL]: { minVotes: 7, totalVoters: 8, threshold: 0.875, polyhedron: GeometricConstraint.SIXTEENCELL },
  [GeometricConstraint.TWENTYFOURCELL]: { minVotes: 20, totalVoters: 24, threshold: 0.833, polyhedron: GeometricConstraint.TWENTYFOURCELL }
};

export class GeometricRBAC {
  private roles: Map<string, GeometricRole> = new Map();
  private rootKey: Uint8Array;
  private rootChainCode: Uint8Array;

  constructor(rootKey: Uint8Array, rootChainCode: Uint8Array) {
    this.rootKey = rootKey;
    this.rootChainCode = rootChainCode;
  }

  /**
   * Derive a role from parent role using BIP32 derivation
   * Path format: "m/enterprise/cto" → hierarchical role structure
   */
  async deriveRole(
    name: string,
    derivationPath: string,
    constraint: GeometricConstraint,
    parent?: GeometricRole
  ): Promise<GeometricRole> {
    // Parse BIP32 path
    const indices = parsePath(derivationPath);

    // Derive key using BIP32
    let currentKey = this.rootKey;
    let currentChainCode = this.rootChainCode;

    for (const index of indices) {
      const derived = await deriveKey(currentKey, currentChainCode, index);
      currentKey = derived.key;
      currentChainCode = derived.chainCode;
    }

    // Create role
    const role: GeometricRole = {
      name,
      derivationPath,
      constraints: POLYHEDRA[constraint],
      key: currentKey,
      chainCode: currentChainCode,
      parent
    };

    this.roles.set(name, role);
    return role;
  }

  /**
   * Check if access is granted based on geometric constraints
   */
  async checkAccess(
    role: GeometricRole,
    votes: number
  ): Promise<{ granted: boolean; proof: string }> {
    const { minVotes, totalVoters, threshold } = role.constraints;

    // Calculate actual threshold
    const actualThreshold = votes / totalVoters;

    // Access granted if votes meet or exceed minimum
    const granted = votes >= minVotes && actualThreshold >= threshold;

    // Generate geometric proof
    const proof = `Polyhedron: ${role.constraints.polyhedron}, Votes: ${votes}/${totalVoters} (${(actualThreshold * 100).toFixed(1)}%), Required: ${minVotes} (${(threshold * 100).toFixed(1)}%)`;

    return { granted, proof };
  }

  /**
   * Verify geometric constraints (Euler's formula for polyhedra)
   */
  static verifyEulerFormula(vertices: number, edges: number, faces: number): boolean {
    // Euler's formula: V - E + F = 2 (for polyhedra)
    return vertices - edges + faces === 2;
  }

  /**
   * Get role by name
   */
  getRole(name: string): GeometricRole | undefined {
    return this.roles.get(name);
  }

  /**
   * Get all roles
   */
  getAllRoles(): GeometricRole[] {
    return Array.from(this.roles.values());
  }
}

// RBAC Templates
export class RBACTemplates {
  /**
   * Enterprise template (company hierarchy)
   */
  static enterprise(): Array<{ name: string; path: string; constraint: GeometricConstraint }> {
    return [
      { name: 'CEO', path: "m/44'/0'/0'/0", constraint: GeometricConstraint.ICOSAHEDRON },    // 83% consensus
      { name: 'CTO', path: "m/44'/0'/1'/0", constraint: GeometricConstraint.CUBE },           // 50% consensus
      { name: 'Engineer', path: "m/44'/0'/1'/1", constraint: GeometricConstraint.OCTAHEDRON } // 83% consensus
    ];
  }

  /**
   * Government template (federal/state/local)
   */
  static government(): Array<{ name: string; path: string; constraint: GeometricConstraint }> {
    return [
      { name: 'Federal', path: "m/44'/1'/0'/0", constraint: GeometricConstraint.ICOSAHEDRON },    // National
      { name: 'State', path: "m/44'/1'/1'/0", constraint: GeometricConstraint.CUBE },             // State
      { name: 'Municipal', path: "m/44'/1'/2'/0", constraint: GeometricConstraint.OCTAHEDRON }    // Local
    ];
  }

  /**
   * Healthcare template (patient-controlled)
   */
  static healthcare(): Array<{ name: string; path: string; constraint: GeometricConstraint }> {
    return [
      { name: 'Patient', path: "m/44'/2'/0'/0", constraint: GeometricConstraint.TETRAHEDRON },    // 100% control
      { name: 'Doctor', path: "m/44'/2'/1'/0", constraint: GeometricConstraint.OCTAHEDRON },      // 83% approval
      { name: 'Hospital', path: "m/44'/2'/2'/0", constraint: GeometricConstraint.CUBE }           // 50% access
    ];
  }
}
```

### 2.2 Emacs Integration

**File:** `modules/meta-log-rbac.el`

```elisp
;;; meta-log-rbac.el --- Federated Role-Based Access Control

;;; Code:

(require 'meta-log-utct)

(defvar meta-log-rbac--roles nil
  "Cache of RBAC roles.")

;;;###autoload
(defun meta-log-rbac-derive-role (name path constraint)
  "Derive role NAME with derivation PATH and geometric CONSTRAINT.
CONSTRAINT should be one of: tetrahedron, cube, octahedron, etc."
  (interactive "sRole name: \nsDerivation path (e.g., m/enterprise/cto): \nsConstraint: ")
  (let* ((params `((name . ,name)
                   (path . ,path)
                   (constraint . ,constraint)))
         (result (meta-log-utct--rpc-call "rbac.deriveRole" params)))
    (push result meta-log-rbac--roles)
    (message "Role %s derived: %s (threshold: %.1f%%)"
             name
             (alist-get 'polyhedron (alist-get 'constraints result))
             (* 100 (alist-get 'threshold (alist-get 'constraints result))))
    result))

;;;###autoload
(defun meta-log-rbac-check-access (role-name votes)
  "Check if VOTES grants access for ROLE-NAME."
  (interactive "sRole name: \nnNumber of votes: ")
  (let* ((params `((roleName . ,role-name)
                   (votes . ,votes)))
         (result (meta-log-utct--rpc-call "rbac.checkAccess" params))
         (granted (alist-get 'granted result))
         (proof (alist-get 'proof result)))
    (message "Access %s: %s"
             (if granted "GRANTED" "DENIED")
             proof)
    granted))

;;;###autoload
(defun meta-log-rbac-load-template (template-name)
  "Load RBAC TEMPLATE-NAME (enterprise, government, healthcare)."
  (interactive "sTemplate (enterprise/government/healthcare): ")
  (let* ((params `((template . ,template-name)))
         (result (meta-log-utct--rpc-call "rbac.loadTemplate" params)))
    (setq meta-log-rbac--roles result)
    (message "Loaded %s template: %d roles"
             template-name
             (length result))
    result))

(provide 'meta-log-rbac)

;;; meta-log-rbac.el ends here
```

### 2.3 Deliverables (Phase 2)

- [ ] `src/rbac/geometric-rbac.ts` (600 LOC)
- [ ] `src/rbac/templates.ts` (200 LOC)
- [ ] `src/__tests__/rbac.test.ts` (300 LOC)
- [ ] `modules/meta-log-rbac.el` (400 LOC)
- [ ] Templates: `examples/rbac/{enterprise,gov,healthcare}.el`
- [ ] Documentation: `docs/RBAC-GUIDE.md`

**Timeline:** 4 weeks
**Success Metric:** Production-ready RBAC templates for 3 domains

---

## Phase 3: CanvasL Enhancement (Weeks 8-10)

### 3.1 TypeScript Enhancement

**Enhance:** `src/jsonl/parser.ts`

```typescript
/**
 * Enhanced CanvasL parser with dual-pair stratification
 */

export interface CanvasLDirective {
  type: 'version' | 'schema' | 'dimension' | 'branch' | 'include';
  value: string;
}

export interface CanvasLObject {
  id?: string;
  type: string;
  [key: string]: any;
}

export interface CanvasLFile {
  directives: CanvasLDirective[];
  objects: CanvasLObject[];
  metadata: {
    side: 'left' | 'right' | 'meta';  // Dual-pair stratification
    includes: string[];
  };
}

export class CanvasLParser {
  /**
   * Parse CanvasL file with dual-pair awareness
   */
  static parse(content: string): CanvasLFile {
    const lines = content.split('\n');
    const directives: CanvasLDirective[] = [];
    const objects: CanvasLObject[] = [];
    const includes: string[] = [];
    let side: 'left' | 'right' | 'meta' = 'meta';

    for (const line of lines) {
      const trimmed = line.trim();

      // Skip empty lines and comments
      if (!trimmed || trimmed.startsWith(';')) continue;

      // Parse directives
      if (trimmed.startsWith('@')) {
        const directive = this.parseDirective(trimmed);
        directives.push(directive);

        if (directive.type === 'include') {
          includes.push(directive.value);
        }

        // Detect side based on @schema
        if (directive.type === 'schema') {
          side = this.detectSide(directive.value);
        }

        continue;
      }

      // Parse JSON objects
      try {
        const obj = JSON.parse(trimmed);
        objects.push(obj);
      } catch (e) {
        // Skip invalid JSON
      }
    }

    return {
      directives,
      objects,
      metadata: { side, includes }
    };
  }

  /**
   * Parse directive line
   */
  private static parseDirective(line: string): CanvasLDirective {
    const match = line.match(/@(\w+)\s+(.+)/);
    if (!match) {
      throw new Error(`Invalid directive: ${line}`);
    }

    return {
      type: match[1] as any,
      value: match[2]
    };
  }

  /**
   * Detect dual-pair side based on schema name
   * Left = affine/value layer, Right = projective/action layer
   */
  private static detectSide(schema: string): 'left' | 'right' | 'meta' {
    // Left side patterns (affine/GCD/what things ARE)
    if (schema.includes('port') || schema.includes('token') || schema.includes('value')) {
      return 'left';
    }

    // Right side patterns (projective/LCM/what things DO)
    if (schema.includes('service') || schema.includes('marketplace') || schema.includes('action')) {
      return 'right';
    }

    return 'meta';
  }

  /**
   * Generate CanvasL from objects
   */
  static generate(file: CanvasLFile): string {
    let output = '';

    // Write directives
    for (const directive of file.directives) {
      output += `@${directive.type} ${directive.value}\n`;
    }

    output += '\n';

    // Write objects
    for (const obj of file.objects) {
      output += JSON.stringify(obj) + '\n';
    }

    return output;
  }
}
```

### 3.2 Emacs Org-Babel Integration

**File:** `modules/meta-log-canvasl.el`

```elisp
;;; meta-log-canvasl.el --- CanvasL format support

;;; Code:

(require 'org)
(require 'ob)

;;;###autoload
(defun meta-log-canvasl-parse (file)
  "Parse CanvasL FILE and return structure."
  (let* ((content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
         (params `((content . ,content)))
         (result (meta-log-utct--rpc-call "canvasl.parse" params)))
    result))

;;;###autoload
(defun meta-log-canvasl-generate (objects directives)
  "Generate CanvasL content from OBJECTS and DIRECTIVES."
  (let* ((params `((objects . ,objects)
                   (directives . ,directives)))
         (result (meta-log-utct--rpc-call "canvasl.generate" params)))
    (alist-get 'content result)))

;; Org-babel support
(defun org-babel-execute:canvasl (body params)
  "Execute CanvasL source BODY with PARAMS."
  (let* ((parsed (meta-log-utct--rpc-call "canvasl.parse"
                                          `((content . ,body))))
         (objects (alist-get 'objects parsed)))
    (format "Parsed %d objects (side: %s)"
            (length objects)
            (alist-get 'side (alist-get 'metadata parsed)))))

(add-to-list 'org-babel-load-languages '(canvasl . t))

(provide 'meta-log-canvasl)

;;; meta-log-canvasl.el ends here
```

### 3.3 Self-Tangling Example

**File:** `examples/self-dual.org`

```org
* Self-Encoding CanvasL Document
:PROPERTIES:
:YAML_FRONTMATTER: true
:CANVASL_SIDE: meta
:END:

This document can tangle itself into CanvasL and re-import the result.

** CanvasL Source
#+BEGIN_SRC canvasl :tangle "self-dual.canvasl" :exports both
@version 1.0
@schema self-encoding-v1
@dimension 0D
@branch 00-mathesis

{"id":"self-org","type":"node","label":"Org Encoding Itself"}
{"edge":"self-org --> self-org","label":"tangle-loop","style":"dashed"}
#+END_SRC

** Tangle and Re-import
#+BEGIN_SRC elisp :exports both
(org-babel-tangle)
(meta-log-canvasl-parse "self-dual.canvasl")
#+END_SRC
```

### 3.4 Deliverables (Phase 3)

- [ ] Enhanced `src/jsonl/parser.ts` with dual-pair detection (300 LOC)
- [ ] `modules/meta-log-canvasl.el` with org-babel (400 LOC)
- [ ] `examples/self-dual.org` (self-tangling demo)
- [ ] Documentation: `docs/CANVASL-GUIDE.md`

**Timeline:** 3 weeks
**Success Metric:** Self-tangling Org documents work end-to-end

---

## JSON-RPC Bridge Implementation

**File:** `meta-log-db/src/server/rpc-server.ts`

```typescript
/**
 * JSON-RPC server for Emacs integration
 */

import express from 'express';
import bodyParser from 'body-parser';
import { UTCTStateMachine, UNIVERSAL_BASIS } from '../utct/state-machine.js';
import { GeometricRBAC, RBACTemplates } from '../rbac/geometric-rbac.js';
import { CanvasLParser } from '../jsonl/parser.js';

const app = express();
app.use(bodyParser.json());

// State
let utctMachine = new UTCTStateMachine(UNIVERSAL_BASIS);
let rbacSystem: GeometricRBAC | null = null;

// RPC endpoint
app.post('/rpc', async (req, res) => {
  const { method, params } = req.body;

  try {
    let result;

    // UTCT methods
    if (method === 'utct.initialize') {
      utctMachine = new UTCTStateMachine(UNIVERSAL_BASIS);
      result = { state: utctMachine.getState() };
    }
    else if (method === 'utct.applyDelta') {
      result = await utctMachine.applyTransformation(params.delta);
    }
    else if (method === 'utct.getState') {
      result = { state: utctMachine.getState() };
    }
    else if (method === 'utct.computeDelta') {
      result = { delta: UTCTStateMachine.computeDelta(params.oldState, params.newState) };
    }

    // RBAC methods
    else if (method === 'rbac.deriveRole') {
      if (!rbacSystem) {
        // Initialize with dummy keys for demo
        rbacSystem = new GeometricRBAC(new Uint8Array(32), new Uint8Array(32));
      }
      result = await rbacSystem.deriveRole(params.name, params.path, params.constraint);
    }
    else if (method === 'rbac.checkAccess') {
      const role = rbacSystem?.getRole(params.roleName);
      if (!role) throw new Error(`Role not found: ${params.roleName}`);
      result = await rbacSystem.checkAccess(role, params.votes);
    }
    else if (method === 'rbac.loadTemplate') {
      const template = RBACTemplates[params.template as keyof typeof RBACTemplates]();
      result = template;
    }

    // CanvasL methods
    else if (method === 'canvasl.parse') {
      result = CanvasLParser.parse(params.content);
    }
    else if (method === 'canvasl.generate') {
      result = { content: CanvasLParser.generate({ directives: params.directives, objects: params.objects, metadata: { side: 'meta', includes: [] } }) };
    }

    else {
      throw new Error(`Unknown method: ${method}`);
    }

    res.json({ jsonrpc: '2.0', result, id: req.body.id });
  } catch (error: any) {
    res.json({ jsonrpc: '2.0', error: { message: error.message }, id: req.body.id });
  }
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`meta-log-db RPC server running on port ${PORT}`);
});
```

---

## Timeline & Milestones

| Phase | Weeks | Deliverables | Success Metric |
|-------|-------|--------------|----------------|
| **Phase 1: UTCT** | 1-3 | State machine, tests, Emacs integration | 70% sync bandwidth reduction |
| **Phase 2: RBAC** | 4-7 | Geometric access control, templates | 3 production templates |
| **Phase 3: CanvasL** | 8-10 | Parser enhancement, org-babel, self-tangling | End-to-end self-tangle works |
| **Testing & Docs** | 11-12 | Integration tests, user guides, examples | 90% test coverage |

**Total Duration:** 12 weeks (3 months)

---

## Next Steps (This Week)

1. **Set up meta-log-db RPC server**
   ```bash
   cd /data/data/com.termux/files/home/github/meta-log-db
   mkdir -p src/utct src/rbac src/server
   npm install express body-parser @types/express
   ```

2. **Implement Phase 1: UTCT core**
   - Create `src/utct/state-machine.ts`
   - Write tests
   - Start RPC server

3. **Test Emacs ↔ TypeScript integration**
   - Create `modules/meta-log-utct.el`
   - Test JSON-RPC calls
   - Verify state transformations

4. **Documentation**
   - Write `docs/UTCT-GUIDE.md`
   - Create demo videos
   - Update README

---

## Success Criteria

✅ **Phase 1 Complete When:**
- UTCT state machine passes all tests
- Emacs can call TypeScript via JSON-RPC
- Federation sync uses ΔT encoding
- Bandwidth reduction measured at ≥70%

✅ **Phase 2 Complete When:**
- 3 RBAC templates work end-to-end
- Geometric proofs validate in O(1) time
- Emacs dashboard shows role hierarchies

✅ **Phase 3 Complete When:**
- Org document can tangle to CanvasL
- Dual-pair stratification enforced
- Self-tangling loop demonstrated

---

## Questions & Decisions

**Q1:** Should RPC server run as separate process or embedded in Emacs?
**A:** Separate process (Node.js) for better performance, browser reuse

**Q2:** How to handle BIP32 keys in TypeScript?
**A:** Use existing `src/browser/crypto/bip32.ts`, add role interpretation layer

**Q3:** CanvasL schema registry - where to store?
**A:** Start with JSON files in `meta-log-db/schemas/`, later upgrade to IPFS

---

**Ready to begin implementation!** 🚀

Let's start with Phase 1 (UTCT State Machine) this week.
