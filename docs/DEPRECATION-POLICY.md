# Deprecation Policy

**Version**: 1.0  
**Date**: 2025-01-XX  
**Purpose**: Formal process for deprecating code and managing breaking changes

---

## Overview

This document defines the formal process for deprecating code in the meta-log codebase, including timelines, communication, and migration paths.

## Deprecation Process

### 1. Deprecation Announcement

When code is marked for deprecation:

1. **Add deprecation marker** in code:
   ```elisp
   ;; @deprecated Since version X.Y.Z - Use new-function instead
   (defun old-function ...)
   ```

2. **Update documentation**:
   - Add deprecation notice to function docstring
   - Update API-REFERENCE.md with deprecation status
   - Add entry to DEPRECATED.md

3. **Announce in release notes**:
   - Document in RELEASE_NOTES for the version
   - Provide migration guide

### 2. Deprecation Timeline

**Standard Timeline**: 2 major versions (approximately 6-12 months)

- **Version N**: Deprecation announced
- **Version N+1**: Deprecation warning in code
- **Version N+2**: Code removed

**Exceptions**:
- Security issues: Immediate removal (0 versions)
- Critical bugs: 1 version (3-6 months)
- Experimental features: 1 version (3-6 months)

### 3. Deprecation Warnings

Deprecated functions should emit warnings:

```elisp
(defun old-function (arg)
  "Old function (DEPRECATED: use new-function instead)."
  (display-warning 'meta-log
                   (format "old-function is deprecated, use new-function instead")
                   :warning)
  (new-function arg))
```

### 4. Migration Guides

For each deprecated function, provide:

1. **Replacement function**: Clear alternative
2. **Migration example**: Before/after code
3. **Breaking changes**: What changed and why

Example:
```markdown
## old-function → new-function

**Deprecated**: Version 1.0.0  
**Removed**: Version 3.0.0  
**Replacement**: `new-function`

### Before
```elisp
(old-function arg1 arg2)
```

### After
```elisp
(new-function arg1 arg2 :option value)
```

### Changes
- Added `:option` parameter for better control
- Improved error handling
```

## Current Deprecations

### automaton-evolutions Exports

**Status**: Deprecated  
**Since**: Version 1.0.0  
**Removed**: Version 3.0.0 (planned)  
**Location**: `automaton-evolutions/src/index.ts`

**Deprecated Functions**:
- `getPrimaryAutomatonFiles` (legacy alias)
- `getPrimaryAutomatonFilesSync` (legacy alias)

**Replacement**: Use `getAutomatonFiles` and `getAutomatonFilesSync` instead.

**Migration**:
```typescript
// Before
import { getPrimaryAutomatonFiles } from 'automaton-evolutions';

// After
import { getAutomatonFiles } from 'automaton-evolutions';
```

## Deprecation Markers

### Code Markers

**Emacs Lisp**:
```elisp
;; @deprecated Since 1.0.0 - Use new-function instead
(defun old-function ...)
```

**Scheme**:
```scheme
;; @deprecated Since 1.0.0 - Use new-function instead
(define (old-function ...) ...)
```

**TypeScript/JavaScript**:
```typescript
/**
 * @deprecated Since 1.0.0 - Use newFunction instead
 */
export function oldFunction() { ... }
```

**Python**:
```python
# @deprecated Since 1.0.0 - Use new_function instead
def old_function():
    ...
```

## Removal Process

Before removing deprecated code:

1. **Verify no usage**: Search codebase for references
2. **Update tests**: Remove or update tests for deprecated code
3. **Update documentation**: Remove from API-REFERENCE.md
4. **Announce removal**: Document in release notes
5. **Remove code**: Delete deprecated functions
6. **Update DEPRECATED.md**: Mark as removed

## Exceptions

### Immediate Removal

Code may be removed immediately without deprecation period if:

- Security vulnerability
- Critical bug causing data loss
- Legal/compliance issue
- Experimental feature marked as unstable

### Extended Deprecation

Deprecation period may be extended if:

- Wide usage in external projects
- Complex migration path
- Breaking changes affect many modules

## Communication

### Release Notes

All deprecations must be documented in release notes:

```markdown
## Deprecations

### old-function (since 1.0.0)

**Status**: Deprecated, will be removed in 3.0.0  
**Replacement**: `new-function`

See [Migration Guide](MIGRATION-GUIDE.md#old-function) for details.
```

### Migration Guide

Create or update `docs/MIGRATION-GUIDE.md` with:

- List of all deprecated functions
- Migration examples
- Breaking changes
- Timeline for removal

## Review Process

### Regular Review

- **Quarterly**: Review all deprecated code
- **Before major release**: Assess removal readiness
- **After removal**: Verify no regressions

### Deprecation Requests

To request deprecation:

1. Open issue with label `deprecation`
2. Provide justification
3. Propose replacement
4. Get approval from maintainers
5. Follow deprecation process

## Best Practices

1. **Minimize breaking changes**: Prefer adding new functions over removing old ones
2. **Clear communication**: Document deprecations clearly
3. **Provide alternatives**: Always provide replacement functions
4. **Test migrations**: Test migration paths before deprecation
5. **Monitor usage**: Track usage of deprecated code

## Related Documents

- [DEPRECATED.md](DEPRECATED.md) - Complete list of deprecated code
- [MIGRATION-GUIDE.md](MIGRATION-GUIDE.md) - Migration instructions
- [API-REFERENCE.md](API-REFERENCE.md) - API documentation with deprecation status
- [RELEASE_NOTES](RELEASE_NOTES_v1.0.0-e8.md) - Release notes with deprecations

---

**Last Updated**: 2025-01-XX  
**Next Review**: Quarterly or before major releases

