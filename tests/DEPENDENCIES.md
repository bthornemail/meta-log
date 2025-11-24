# Test Dependencies

**Date**: 2025-01-XX  
**Purpose**: Document all test dependencies and installation instructions

---

## Overview

This document lists all dependencies required to run the meta-log test suite, including installation instructions and service startup requirements.

---

## Core Dependencies

### Python Dependencies

**Required**:
- Python 3.8+ (tested with Python 3.10+)
- `pytest` (version 7.4.3+)
- `pytest-cov` (version 4.1.0+) - for coverage reporting
- `pytest-asyncio` (version 0.21.1+) - for async tests
- `fastapi` (version 0.104.1+) - for API services
- `uvicorn` (version 0.24.0+) - for running FastAPI services
- `numpy` (version 1.26.2+) - for scientific computing
- `opencv-python` (version 4.8.1.78+) - for computer vision
- `Pillow` (version 10.1.0+) - for image processing
- `requests` (version 2.31.0+) - for HTTP client

**Installation**:
```bash
pip install -r requirements.txt
```

### Emacs Lisp Dependencies

**Required**:
- Emacs 27.1+ (tested with Emacs 28+)
- `ERT` (Emacs Regression Testing) - included with Emacs
- `undercover.el` - for coverage reporting (optional)

**Installation**:
```bash
# ERT is included with Emacs, no installation needed

# Install undercover.el (optional, for coverage)
# In Emacs: M-x package-install RET undercover RET
# Or add to your init.el:
# (use-package undercover :ensure t)
```

### Scheme Dependencies

**Required**:
- Guile 3.0+ (for R5RS Scheme execution)
- Geiser (Emacs package for Scheme integration)

**Installation**:
```bash
# Guile (Ubuntu/Debian)
sudo apt-get install guile-3.0

# Guile (macOS)
brew install guile

# Geiser (in Emacs)
M-x package-install RET geiser RET
```

---

## Service Dependencies

### FastAPI Services

The following services need to be running for integration tests:

#### E8 API Service

**Location**: `services/e8-api/`

**Start Command**:
```bash
cd services/e8-api
uvicorn main:app --host 0.0.0.0 --port 8000
```

**Health Check**: `http://localhost:8000/health`

#### Vision API Service

**Location**: `services/vision-api/`

**Start Command**:
```bash
cd services/vision-api
uvicorn main:app --host 0.0.0.0 --port 8001
```

**Health Check**: `http://localhost:8001/health`

#### Quantum Simulation Service

**Location**: `services/quantum-simulation/`

**Start Command**:
```bash
cd services/quantum-simulation
uvicorn main:app --host 0.0.0.0 --port 8002
```

**Health Check**: `http://localhost:8002/health`

#### Sensors API Service

**Location**: `services/sensors-api/`

**Start Command**:
```bash
cd services/sensors-api
uvicorn main:app --host 0.0.0.0 --port 8003
```

**Health Check**: `http://localhost:8003/health`

**Note**: Sensors API may require actual hardware (GPS, motion sensors) or will use mock data.

---

## Test Execution Scripts

### Comprehensive Test Runner

**Script**: `tests/run-all-tests-with-deps.sh`

**Features**:
- Checks for all dependencies
- Starts required services automatically
- Runs tests in correct order
- Generates combined test report

**Usage**:
```bash
./tests/run-all-tests-with-deps.sh
```

### Python Test Runner

**Script**: `scripts/run-python-coverage.sh`

**Usage**:
```bash
./scripts/run-python-coverage.sh
```

### Emacs Lisp Test Runner

**Script**: `scripts/run-elisp-coverage.sh`

**Usage**:
```bash
./scripts/run-elisp-coverage.sh
```

---

## Dependency Checking

### Check Python Dependencies

```bash
python3 -c "import pytest, pytest_cov, fastapi, uvicorn, numpy, cv2, PIL, requests; print('All Python dependencies installed')"
```

### Check Emacs Dependencies

```bash
emacs --batch --eval "(require 'ert) (require 'undercover) (message 'All Emacs dependencies installed')"
```

### Check Guile

```bash
guile --version
```

### Check Services

```bash
# Check if services are running
curl http://localhost:8000/health  # E8 API
curl http://localhost:8001/health  # Vision API
curl http://localhost:8002/health  # Quantum Simulation
curl http://localhost:8003/health  # Sensors API
```

---

## Installation Instructions

### Full Installation (All Dependencies)

```bash
# 1. Install Python dependencies
pip install -r requirements.txt

# 2. Install Guile (if not already installed)
# Ubuntu/Debian:
sudo apt-get install guile-3.0

# macOS:
brew install guile

# 3. Install Emacs packages (in Emacs)
M-x package-install RET geiser RET
M-x package-install RET undercover RET  # Optional, for coverage
```

### Minimal Installation (Core Tests Only)

```bash
# Install only Python dependencies for core tests
pip install pytest pytest-asyncio fastapi uvicorn
```

---

## Service Startup

### Manual Startup

Start each service in a separate terminal:

```bash
# Terminal 1: E8 API
cd services/e8-api && uvicorn main:app --port 8000

# Terminal 2: Vision API
cd services/vision-api && uvicorn main:app --port 8001

# Terminal 3: Quantum Simulation
cd services/quantum-simulation && uvicorn main:app --port 8002

# Terminal 4: Sensors API
cd services/sensors-api && uvicorn main:app --port 8003
```

### Automatic Startup

The `tests/run-all-tests-with-deps.sh` script will start services automatically if they're not running.

---

## Troubleshooting

### Python Dependencies

**Issue**: `ModuleNotFoundError: No module named 'pytest'`

**Solution**:
```bash
pip install -r requirements.txt
```

### Emacs Dependencies

**Issue**: `Error: Cannot find ERT`

**Solution**: ERT is included with Emacs. If missing, update Emacs or install from package manager.

**Issue**: `Error: Cannot find undercover.el`

**Solution**: Install with `M-x package-install RET undercover RET` (optional, only needed for coverage)

### Guile Dependencies

**Issue**: `guile: command not found`

**Solution**:
```bash
# Ubuntu/Debian
sudo apt-get install guile-3.0

# macOS
brew install guile
```

### Service Dependencies

**Issue**: `Connection refused` when accessing services

**Solution**: Ensure services are running:
```bash
# Check if services are running
ps aux | grep uvicorn

# Start services if not running
./tests/run-all-tests-with-deps.sh
```

---

## CI/CD Dependencies

For CI/CD environments, ensure:

1. **Python**: Python 3.8+ is available
2. **Emacs**: Emacs 27.1+ is available (for ERT tests)
3. **Guile**: Guile 3.0+ is available (for Scheme tests)
4. **Services**: Services can be started in background

**Example CI Configuration**:
```yaml
# GitHub Actions example
- name: Install Python dependencies
  run: pip install -r requirements.txt

- name: Install Guile
  run: sudo apt-get install -y guile-3.0

- name: Start services
  run: |
    cd services/e8-api && uvicorn main:app --port 8000 &
    cd services/vision-api && uvicorn main:app --port 8001 &
```

---

## Version Compatibility

### Tested Versions

- **Python**: 3.10, 3.11, 3.12
- **Emacs**: 27.1, 28.1, 29.1
- **Guile**: 3.0, 3.0.8
- **pytest**: 7.4.3+
- **FastAPI**: 0.104.1+

### Minimum Versions

- **Python**: 3.8
- **Emacs**: 27.1
- **Guile**: 3.0
- **pytest**: 7.0
- **FastAPI**: 0.100.0

---

## Additional Resources

- [pytest Documentation](https://docs.pytest.org/)
- [ERT Documentation](https://www.gnu.org/software/emacs/manual/html_node/ert/)
- [Guile Documentation](https://www.gnu.org/software/guile/manual/)
- [FastAPI Documentation](https://fastapi.tiangolo.com/)
- [undercover.el Documentation](https://github.com/sviridov/undercover.el)

---

**Last Updated**: 2025-01-XX


