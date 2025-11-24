# Deprecated Code Inventory

**Date:** 2025-01-XX  
**Audit Scope:** Complete inventory of deprecated, legacy, and obsolete code in meta-log codebase

---

## Executive Summary

### Deprecated Code Summary

| Category | Count | Status | Removal Priority |
|----------|-------|--------|------------------|
| **Explicit Deprecations** | 5 | Documented | Low (backward compatibility) |
| **Placeholder/Stub Functions** | 20+ | Needs implementation | High |
| **Unused Functions** | Unknown | Needs analysis | Medium |
| **Legacy Code** | 0 | None found | N/A |

### Key Findings

1. **Explicit Deprecations**: ✅ Well-documented in TypeScript code
2. **Placeholder Functions**: ⚠️ Many functions marked as stubs/placeholders
3. **Unused Code**: ⚠️ No systematic analysis performed
4. **Legacy Code**: ✅ No legacy code identified

---

## Explicit Deprecations

### automaton-evolutions Package

**Location**: `automaton-evolutions/src/index.ts`

#### Deprecated Exports

The following exports are deprecated and maintained for backward compatibility only:

1. **`unified`** (deprecated)
   - **Replacement**: `AUTOMATON_FILES.a0Unified`
   - **Status**: ✅ Documented with `@deprecated` JSDoc
   - **Removal Timeline**: TBD
   - **Migration**: Use `AUTOMATON_FILES.a0Unified` instead

2. **`kernelSeed`** (deprecated)
   - **Replacement**: `AUTOMATON_FILES.a1KernelSeed`
   - **Status**: ✅ Documented with `@deprecated` JSDoc
   - **Removal Timeline**: TBD
   - **Migration**: Use `AUTOMATON_FILES.a1KernelSeed` instead

3. **`shape`** (deprecated)
   - **Replacement**: `AUTOMATON_FILES.a2Shape`
   - **Status**: ✅ Documented with `@deprecated` JSDoc
   - **Removal Timeline**: TBD
   - **Migration**: Use `AUTOMATON_FILES.a2Shape` instead

4. **`centroid`** (deprecated)
   - **Replacement**: `AUTOMATON_FILES.a3Centroid`
   - **Status**: ✅ Documented with `@deprecated` JSDoc
   - **Removal Timeline**: TBD
   - **Migration**: Use `AUTOMATON_FILES.a3Centroid` instead

5. **`basis`** (deprecated)
   - **Replacement**: `AUTOMATON_FILES.a4Basis`
   - **Status**: ✅ Documented with `@deprecated` JSDoc
   - **Removal Timeline**: TBD
   - **Migration**: Use `AUTOMATON_FILES.a4Basis` instead

**Code Reference**:
```typescript
// Legacy aliases for backward compatibility (deprecated)
/** @deprecated Use AUTOMATON_FILES.a0Unified instead */
export const unified = AUTOMATON_FILES.a0Unified;
/** @deprecated Use AUTOMATON_FILES.a1KernelSeed instead */
export const kernelSeed = AUTOMATON_FILES.a1KernelSeed;
/** @deprecated Use AUTOMATON_FILES.a2Shape instead */
export const shape = AUTOMATON_FILES.a2Shape;
/** @deprecated Use AUTOMATON_FILES.a3Centroid instead */
export const centroid = AUTOMATON_FILES.a3Centroid;
/** @deprecated Use AUTOMATON_FILES.a4Basis instead */
export const basis = AUTOMATON_FILES.a4Basis;
```

**Documentation**: `automaton-evolutions/docs/api.md` documents these as deprecated

---

## Placeholder/Stub Functions

### MLSS Substrate System

#### Scheme Modules

**Location**: `scheme/` directory

1. **`call-vision-api`** (`scheme/vision/features.scm`)
   - **Status**: Placeholder - raises error
   - **Issue**: FastAPI vision service not yet implemented
   - **Priority**: Medium
   - **Code**: `(error "FastAPI vision service not yet implemented")`

2. **`extract-sift`** (`scheme/vision/features.scm`)
   - **Status**: Placeholder - calls non-functional `call-vision-api`
   - **Issue**: Depends on FastAPI service
   - **Priority**: Medium

3. **`extract-orb`** (`scheme/vision/features.scm`)
   - **Status**: Placeholder - calls non-functional `call-vision-api`
   - **Issue**: Depends on FastAPI service
   - **Priority**: Medium

4. **`match-features-api`** (`scheme/vision/features.scm`)
   - **Status**: Placeholder - calls non-functional `call-vision-api`
   - **Issue**: Depends on FastAPI service
   - **Priority**: Medium

5. **`waveform-compute-fft`** (`scheme/substrate/waveform.scm`)
   - **Status**: Placeholder - returns placeholder structure
   - **Issue**: Should call FastAPI waveform-dsp service
   - **Priority**: Medium
   - **Code**: Returns `'((coefficients . #f) (fft-size . ...) (computed . #t))`

6. **`waveform-compute-padic-signature`** (`scheme/substrate/waveform.scm`)
   - **Status**: Placeholder - returns placeholder structure
   - **Issue**: Needs full p-adic computation
   - **Priority**: Low

7. **`waveform-compute-e8-signature`** (`scheme/substrate/waveform.scm`)
   - **Status**: Placeholder - returns placeholder structure
   - **Issue**: Needs E8 projection computation
   - **Priority**: Medium

8. **`prolog-query`** (`scheme/substrate/prolog-interface.scm`)
   - **Status**: Placeholder - returns hardcoded values
   - **Issue**: Should call meta-log-prolog-query via FFI
   - **Priority**: High
   - **Code**: `(list '((?X . value1) (?Y . value2)))`

9. **`prolog-add-fact`** (`scheme/substrate/prolog-interface.scm`)
   - **Status**: Placeholder - always returns #t
   - **Issue**: Should call meta-log-prolog-add-fact via FFI
   - **Priority**: High

10. **`prolog-add-rule`** (`scheme/substrate/prolog-interface.scm`)
    - **Status**: Placeholder - always returns #t
    - **Issue**: Should call meta-log-prolog-add-rule via FFI
    - **Priority**: High

11. **`datalog-query`** (`scheme/substrate/prolog-interface.scm`)
    - **Status**: Placeholder - returns hardcoded values
    - **Issue**: Should call meta-log-datalog-query via FFI
    - **Priority**: High
    - **Code**: `(list '((?X . value1) (?Y . value2)))`

12. **`datalog-add-fact`** (`scheme/substrate/prolog-interface.scm`)
    - **Status**: Placeholder - always returns #t
    - **Issue**: Should call meta-log-datalog-add-fact via FFI
    - **Priority**: High

13. **`datalog-add-rule`** (`scheme/substrate/prolog-interface.scm`)
    - **Status**: Placeholder - always returns #t
    - **Issue**: Should call meta-log-datalog-add-rule via FFI
    - **Priority**: High

#### Q* Scoring Functions

**Location**: `scheme/qstar/scoring.scm`

✅ **All scoring functions implemented** - See IMPLEMENTATION-STATUS.md for details.

14. ✅ **`qstar-score-euclidean`** - **IMPLEMENTED**
15. ✅ **`qstar-score-weyl`** - **IMPLEMENTED**
16. ✅ **`qstar-score-padic`** - **IMPLEMENTED**
17. ✅ **`qstar-score-rule-compat`** - **IMPLEMENTED**
18. ✅ **`qstar-score-resource`** - **IMPLEMENTED**
19. ✅ **`qstar-score-consensus`** - **IMPLEMENTED**
20. ✅ **`qstar-score-complexity`** - **IMPLEMENTED**

### Emacs Lisp Modules

**Location**: `modules/` directory

21. ✅ **`meta-log-extract-closeness`** (`modules/meta-log-p-adic.el`) - **IMPLEMENTED**
    - Graph closeness centrality computation using BFS

22. ✅ **`meta-log-modular-form-coefficient`** (`modules/meta-log-p-adic.el`) - **IMPLEMENTED**
    - Modular form coefficient extraction

23. ✅ **Helper functions in `meta-log-drinfeld.el`** - **IMPLEMENTED**
    - ✅ `meta-log-drinfeld-reduce-mod-p` - Reduces module coefficients mod prime
    - ✅ `meta-log-drinfeld-special-points` - Extracts special points based on rank
    - ✅ `meta-log-drinfeld-symmetry-group` - Computes symmetry group based on rank

### Additional Stubs Completed

**Location**: `scheme/qstar/` and `modules/`

24. ✅ **Q* Core Cost Functions** (`scheme/qstar/core.scm`) - **IMPLEMENTED**
    - ✅ `qstar-computational-cost` - Computes based on action type and entropy
    - ✅ `qstar-memory-cost` - Computes based on memory usage
    - ✅ `qstar-entropy-cost` - Computes based on entropy increase
    - ✅ `qstar-complexity-cost` - Computes based on system complexity
    - ✅ `qstar-safety-penalty` - Checks safety via Prolog rules

25. ✅ **Q* A* Search** (`scheme/qstar/a-star.scm`) - **IMPLEMENTED**
    - ✅ `get-successors` - Generates successor states from available actions

26. ✅ **Q* Goal/Future** (`scheme/qstar/core.scm`) - **IMPLEMENTED**
    - ✅ `qstar-goal-p` - Checks goal state (high consistency, low entropy)
    - ✅ `qstar-future-value` - Uses A* to compute optimal future value

27. ✅ **WordNet** (`modules/meta-log-wordnet.el`) - **IMPROVED**
    - ✅ `meta-log-wordnet-find-synonyms` - Enhanced with more synonyms, removed TODO

28. ✅ **KG Learning** (`modules/meta-log-kg-learning.el`) - **IMPROVED**
    - ✅ Template generation - Removed TODO, added pattern-based initialization

---

## Implicit Deprecations

### Potentially Unused Code

**Status**: ⚠️ **No systematic analysis performed**

To identify unused code, the following analysis is recommended:

1. **Static Analysis**:
   - Use `elisp-find-unused-functions` for Emacs Lisp
   - Use `grep` to find function references
   - Check for functions defined but never called

2. **Dynamic Analysis**:
   - Run code coverage tools
   - Identify functions never executed in tests
   - Check for dead code paths

3. **Manual Review**:
   - Review commented-out code blocks
   - Check for functions marked as "TODO" or "FIXME"
   - Identify duplicate implementations

### Commented-Out Code

**Status**: ⚠️ **No systematic analysis performed**

No large blocks of commented-out code were found during this audit, but a comprehensive search is recommended.

---

## Removal Recommendations

### High Priority

1. **Implement Placeholder Functions**:
   - Prolog/Datalog interface functions (8 functions)
   - These are critical for MLSS integration
   - **Timeline**: Next release

2. **Replace Stub Functions**:
   - Vision API functions (4 functions)
   - Q* scoring functions (7 functions)
   - **Timeline**: Within 2 releases

### Medium Priority

1. **Document Deprecation Timeline**:
   - Set removal date for deprecated automaton-evolutions exports
   - Provide migration guide
   - **Timeline**: Next release

2. **Remove Unused Code**:
   - After static analysis identifies unused functions
   - **Timeline**: Ongoing cleanup

### Low Priority

1. **Maintain Backward Compatibility**:
   - Keep deprecated automaton-evolutions exports
   - Monitor usage before removal
   - **Timeline**: TBD based on usage

---

## Migration Guides

### automaton-evolutions Deprecated Exports

**Before** (deprecated):
```typescript
import { unified, kernelSeed, shape, centroid, basis } from 'automaton-evolutions';
```

**After** (recommended):
```typescript
import { AUTOMATON_FILES } from 'automaton-evolutions';

// Use AUTOMATON_FILES properties instead
const unified = AUTOMATON_FILES.a0Unified;
const kernelSeed = AUTOMATON_FILES.a1KernelSeed;
const shape = AUTOMATON_FILES.a2Shape;
const centroid = AUTOMATON_FILES.a3Centroid;
const basis = AUTOMATON_FILES.a4Basis;
```

---

## Deprecation Policy

### Current Policy

**Status**: ⚠️ **No formal deprecation policy documented**

### Recommended Policy

1. **Deprecation Period**: Minimum 2 major releases before removal
2. **Documentation**: All deprecations must be documented with:
   - `@deprecated` JSDoc tag (for TypeScript/JavaScript)
   - Deprecation notice in function docstring (for Emacs Lisp)
   - Migration guide in documentation
3. **Warnings**: Emit warnings when deprecated code is used (if possible)
4. **Timeline**: Clear removal timeline in deprecation notice

---

## Summary

### Deprecation Status

- **Explicit Deprecations**: ✅ Well-documented (5 items)
- **Placeholder Functions**: ⚠️ Needs implementation (20+ items)
- **Unused Code**: ⚠️ Needs analysis
- **Legacy Code**: ✅ None found

### Action Items

1. **Immediate**: Document deprecation policy
2. **Short-term**: Implement high-priority placeholder functions
3. **Medium-term**: Complete static analysis for unused code
4. **Long-term**: Establish deprecation timeline for automaton-evolutions exports

---

**Next Steps**: See removal recommendations above for prioritized action items.

