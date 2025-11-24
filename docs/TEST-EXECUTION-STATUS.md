# Test Execution Status

**Date**: 2025-01-XX  
**Purpose**: Track test execution status, dependencies, and frequency

---

## Overview

This document tracks which tests are executed, which require dependencies, and how frequently tests are run.

---

## Test Execution Frequency

| Test Suite | Last Executed | Frequency | Status |
|------------|---------------|-----------|--------|
| Scheme Tests | TBD | On-demand | ⚠️ Requires Guile |
| Python Tests | TBD | On-demand | ⚠️ Requires pytest |
| Emacs Lisp Tests | TBD | On-demand | ⚠️ Requires Emacs/ERT |
| Integration Tests | TBD | On-demand | ⚠️ Requires services |

---

## Test Dependencies

### Required Dependencies

| Dependency | Required For | Status | Installation |
|------------|--------------|--------|--------------|
| Guile 3.0+ | Scheme tests | ⚠️ Check | `sudo apt-get install guile-3.0` |
| pytest | Python tests | ⚠️ Check | `pip install pytest` |
| pytest-cov | Python coverage | ⚠️ Check | `pip install pytest-cov` |
| Emacs 27.1+ | Emacs Lisp tests | ⚠️ Check | `sudo apt-get install emacs` |
| ERT | Emacs Lisp tests | ✅ Included | Part of Emacs |
| uvicorn | Service tests | ⚠️ Check | `pip install uvicorn` |
| FastAPI services | Integration tests | ⚠️ Check | See services/ directories |

### Optional Dependencies

| Dependency | Required For | Status | Installation |
|------------|--------------|--------|--------------|
| undercover.el | Emacs coverage | ⚠️ Optional | `M-x package-install RET undercover RET` |
| Geiser | Scheme integration | ⚠️ Optional | `M-x package-install RET geiser RET` |

---

## Test Execution Scripts

### Comprehensive Test Runner

**Script**: `tests/run-all-tests-with-deps.sh`

**Features**:
- ✅ Checks all dependencies
- ✅ Starts required services automatically
- ✅ Runs tests in correct order
- ✅ Generates combined test report
- ✅ Stops services after tests

**Usage**:
```bash
./tests/run-all-tests-with-deps.sh
```

**Output**:
- Test reports: `tests/test-reports/`
- Coverage reports: `tests/coverage/`
- Execution report: `tests/test-reports/TEST-EXECUTION-REPORT.md`

### Individual Test Runners

**Python Coverage**:
```bash
./scripts/run-python-coverage.sh
```

**Emacs Lisp Coverage**:
```bash
./scripts/run-elisp-coverage.sh
```

---

## Test Execution Status by Category

### Scheme Tests

**Status**: ⚠️ **Partial** - Requires Guile

**Test Files**:
- `scheme/qstar/core.test.scm`
- `scheme/qstar/scoring.test.scm`
- `tests/test-prolog-interface.scm`
- `tests/test-qstar-scoring.scm`
- `tests/test-waveform.scm`

**Dependencies**:
- Guile 3.0+
- Geiser (optional, for Emacs integration)

**Execution**:
```bash
guile -s tests/test-prolog-interface.scm
```

### Python Tests

**Status**: ⚠️ **Partial** - Requires pytest

**Test Directories**:
- `services/substrate-api/tests/`
- `services/e8-api/tests/`
- `services/vision-api/` (test files)
- `services/quantum-simulation/` (test files)
- `services/sensors-api/` (test files)

**Dependencies**:
- pytest
- pytest-cov (for coverage)
- FastAPI services (for integration tests)

**Execution**:
```bash
pytest services/ --cov=services --cov-report=html
```

### Emacs Lisp Tests

**Status**: ⚠️ **Partial** - Requires Emacs/ERT

**Test Files**:
- `tests/test-core.el`
- `tests/test-substrate-runtime.el`
- `tests/test-binary-substrate.el`
- `tests/test-provenance.el`
- And 30+ more test files

**Dependencies**:
- Emacs 27.1+
- ERT (included with Emacs)
- undercover.el (optional, for coverage)

**Execution**:
```bash
emacs --batch --load tests/coverage-config.el --eval "(ert-run-tests-batch-and-exit)"
```

### Integration Tests

**Status**: ⚠️ **Partial** - Requires services

**Requirements**:
- All FastAPI services running
- Network access for API calls

**Services**:
- E8 API (port 8000)
- Vision API (port 8001)
- Quantum Simulation (port 8002)
- Sensors API (port 8003)

**Execution**:
```bash
# Start services first
./tests/run-all-tests-with-deps.sh  # Automatically starts services
```

---

## Test Execution Tracking

### Last Execution Times

| Test Suite | Date | Time | Duration | Result |
|------------|------|------|----------|--------|
| Scheme Tests | TBD | TBD | TBD | TBD |
| Python Tests | TBD | TBD | TBD | TBD |
| Emacs Lisp Tests | TBD | TBD | TBD | TBD |
| Integration Tests | TBD | TBD | TBD | TBD |

### Execution History

| Date | Test Suite | Tests Run | Passed | Failed | Skipped | Notes |
|------|------------|-----------|--------|--------|---------|-------|
| TBD | All | TBD | TBD | TBD | TBD | Baseline |

---

## Dependency Status

### Current System Status

Run dependency check:
```bash
./tests/run-all-tests-with-deps.sh
```

This will check:
- ✅ Guile availability
- ✅ Python3 availability
- ✅ pytest availability
- ✅ pytest-cov availability
- ✅ Emacs availability
- ✅ Service availability

### Installation Status

See `tests/DEPENDENCIES.md` for detailed installation instructions.

---

## Test Execution Recommendations

### Before Committing

1. Run core tests (no dependencies):
   ```bash
   # Structure validation tests
   ```

2. Run tests with available dependencies:
   ```bash
   ./tests/run-all-tests-with-deps.sh
   ```

### Before Releasing

1. Run all tests with all dependencies
2. Generate coverage reports
3. Verify coverage thresholds are met
4. Review test execution report

### CI/CD Integration

**Recommended CI Steps**:
1. Install all dependencies
2. Start required services
3. Run comprehensive test suite
4. Generate coverage reports
5. Upload reports to CI artifacts

---

## Troubleshooting

### Tests Not Running

**Issue**: Tests skipped due to missing dependencies

**Solution**: Install missing dependencies (see `tests/DEPENDENCIES.md`)

### Services Not Starting

**Issue**: FastAPI services fail to start

**Solution**:
- Check if ports are available
- Verify service dependencies are installed
- Check service logs in `tests/test-reports/`

### Coverage Not Generated

**Issue**: Coverage reports not created

**Solution**:
- Verify coverage tools are installed (pytest-cov, undercover.el)
- Check coverage script permissions
- Review coverage configuration files

---

## Next Steps

1. ⚠️ Run baseline test execution
2. ⚠️ Document actual test results
3. ⚠️ Set up CI test execution
4. ⚠️ Track test execution over time
5. ⚠️ Improve test coverage

---

**Last Updated**: 2025-01-XX

