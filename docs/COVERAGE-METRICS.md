# Test Coverage Metrics

**Date**: 2025-01-XX  
**Purpose**: Track test coverage metrics for meta-log codebase

---

## Overview

This document tracks test coverage metrics for the meta-log project, including Python and Emacs Lisp code coverage.

---

## Coverage Tools

### Python Coverage

- **Tool**: `pytest-cov` (version 4.1.0)
- **Configuration**: `pytest.ini`
- **Script**: `scripts/run-python-coverage.sh`
- **Report Location**: `coverage/python/`

### Emacs Lisp Coverage

- **Tool**: `undercover.el`
- **Configuration**: `tests/coverage-config.el`
- **Script**: `scripts/run-elisp-coverage.sh`
- **Report Location**: `coverage/elisp/`

---

## Current Coverage Status

### Python Coverage

**Status**: ⚠️ **Not Yet Measured**

**Target**: 80%+ coverage

**Source Directories**:
- `services/` - FastAPI services (E8 API, Vision API, Quantum Simulation, Sensors API)

**Exclusions**:
- Test files (`*/tests/*`, `*/test_*.py`)
- Cache directories (`*/__pycache__/*`, `*/venv/*`, `*/env/*`)

**To Generate Baseline**:
```bash
./scripts/run-python-coverage.sh
```

### Emacs Lisp Coverage

**Status**: ⚠️ **Not Yet Measured**

**Target**: 80%+ coverage

**Source Directories**:
- `modules/` - Emacs Lisp modules
- Root directory - Core meta-log files

**Exclusions**:
- Test files (`tests/`)
- Documentation (`dev-docs/`, `docs/`)

**To Generate Baseline**:
```bash
./scripts/run-elisp-coverage.sh
```

---

## Coverage Targets

### Short-term (3 months)

- **Python**: 60%+ coverage
- **Emacs Lisp**: 60%+ coverage
- **Overall**: 60%+ coverage

### Medium-term (6 months)

- **Python**: 75%+ coverage
- **Emacs Lisp**: 75%+ coverage
- **Overall**: 75%+ coverage

### Long-term (12 months)

- **Python**: 85%+ coverage
- **Emacs Lisp**: 85%+ coverage
- **Overall**: 85%+ coverage

---

## Coverage History

| Date | Python | Emacs Lisp | Overall | Notes |
|------|--------|------------|---------|-------|
| 2025-01-XX | TBD | TBD | TBD | Baseline measurement pending |

---

## Coverage by Module

### Python Services

| Service | Coverage | Status | Notes |
|---------|----------|--------|-------|
| E8 API | TBD | ⚠️ Not measured | |
| Vision API | TBD | ⚠️ Not measured | |
| Quantum Simulation | TBD | ⚠️ Not measured | |
| Sensors API | TBD | ⚠️ Not measured | |

### Emacs Lisp Modules

| Module Category | Coverage | Status | Notes |
|----------------|----------|--------|-------|
| Core Modules | TBD | ⚠️ Not measured | |
| Optional Modules | TBD | ⚠️ Not measured | |

---

## Improving Coverage

### High Priority Areas

1. **Core MLSS Functions**: Ensure all core substrate functions are tested
2. **Q* Engine**: Test Q* optimality evaluation and cost functions
3. **Bridge Functions**: Test Scheme-to-Emacs-Lisp bridge
4. **API Services**: Test all FastAPI endpoints

### Medium Priority Areas

1. **Optional Modules**: Test high-usage optional modules
2. **Integration Tests**: Test cross-module interactions
3. **Error Handling**: Test error paths and edge cases

### Low Priority Areas

1. **Advanced Features**: Test specialized/advanced functions
2. **Research Features**: Test experimental features

---

## Coverage Reports

### Generating Reports

**Python**:
```bash
./scripts/run-python-coverage.sh
```

**Emacs Lisp**:
```bash
./scripts/run-elisp-coverage.sh
```

### Viewing Reports

**Python HTML Report**:
- Open `coverage/python/html/index.html` in a browser

**Emacs Lisp Report**:
- Check `coverage/elisp/` directory for text/HTML reports

---

## Notes

- Coverage metrics are generated using `pytest-cov` for Python and `undercover.el` for Emacs Lisp
- Coverage thresholds are set to 80% in configuration files
- Reports are generated in `coverage/` directory
- Coverage history is tracked in this document

---

## Next Steps

1. ✅ Configure coverage tools (pytest-cov, undercover.el)
2. ⚠️ Generate baseline coverage reports
3. ⚠️ Set coverage thresholds in CI
4. ⚠️ Track coverage over time
5. ⚠️ Improve coverage for low-coverage areas

---

**Last Updated**: 2025-01-XX


