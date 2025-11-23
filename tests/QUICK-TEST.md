# Quick Test Guide

## Immediate Testing (No Installation Required)

### 1. Verify File Structure

```bash
# Check all components exist
ls -la scheme/substrate/*.scm
ls -la modules/meta-log-substrate*.el
ls -la services/substrate-api/*.py
```

### 2. Syntax Check (Python)

```bash
cd services/substrate-api
python3 -m py_compile app.py && echo "✓ Python syntax OK"
```

### 3. Syntax Check (Emacs Lisp)

```bash
emacs --batch --eval "(byte-compile-file \"modules/meta-log-substrate-runtime.el\")" 2>&1 | grep -v "Warning" || echo "✓ Emacs Lisp syntax OK"
```

### 4. Verify Integration Spec

```bash
# Check integration spec exists and is readable
test -f dev-docs/INTEGRATION-MLSS.md && echo "✓ Integration spec exists"
wc -l dev-docs/INTEGRATION-MLSS.md
```

## With Minimal Setup

### Install FastAPI (if needed)

```bash
pip3 install --user fastapi uvicorn pydantic
```

### Test FastAPI Service

```bash
cd services/substrate-api
python3 app.py &
sleep 2
curl http://localhost:8001/api/v1/health
pkill -f "python3 app.py"
```

## With Full Setup

### Install All Dependencies

```bash
# Guile for Scheme
sudo apt install guile-3.0

# Python dependencies
pip3 install pytest fastapi uvicorn pydantic

# Verify Emacs ERT
emacs --batch --eval "(require 'ert)" && echo "✓ ERT available"
```

### Run All Tests

```bash
./tests/run-substrate-tests.sh
```

---

## Expected Output

When everything is set up correctly:

```
==========================================
MLSS Substrate System Tests
==========================================

Running FastAPI tests...
✓ Health check passed
✓ Hash endpoint works
✓ Compression endpoint works

Running Scheme runtime tests...
✓ UUID generation works
✓ Content hash works
✓ Memory object creation works

Running Emacs Lisp tests...
✓ Substrate runtime tests passed
✓ Binary substrate tests passed
✓ Provenance tests passed
✓ Integration tests passed

==========================================
All tests passed!
```

