---
layout: default
title: Getting Started
nav_order: 2
description: "Quick start guide for meta-log"
permalink: /GETTING-STARTED
---

# Getting Started

This guide will help you get up and running with meta-log in minutes.

## Prerequisites

### Required

- **Emacs 28.1+** - The primary interface for meta-log
- **Guile 3.0+** - R5RS Scheme implementation
- **Python 3.9+** - For FastAPI services (optional but recommended)

### Optional

- **Node.js 20+** - For frontend/automaton components
- **FFmpeg** - For video generation features
- **MQTT Broker** - For federation features

## Quick Installation

### Step 1: Install System Dependencies

**Ubuntu/Debian**:
```bash
sudo apt-get update
sudo apt-get install -y guile-3.0 guile-dev python3 python3-pip python3-venv
```

**macOS**:
```bash
brew install guile python3
```

**Verify**:
```bash
guile --version    # Should be 3.0+
python3 --version  # Should be 3.9+
```

### Step 2: Clone the Repository

```bash
git clone https://github.com/bthornemail/meta-log.git
cd meta-log
```

### Step 3: Install Python Dependencies

```bash
# Create virtual environment
python3 -m venv venv
source venv/bin/activate

# Install dependencies
pip install fastapi uvicorn pydantic numpy scipy
```

Or use the automated script:
```bash
./install-dependencies.sh
```

### Step 4: Install in Emacs

```elisp
;; In Emacs
M-x package-install-file RET /path/to/meta-log/meta-log.el RET
```

Or add to your `load-path`:
```elisp
(add-to-list 'load-path "/path/to/meta-log")
(require 'meta-log)
```

## Your First Steps

### 1. Initialize Meta-Log

```elisp
M-x meta-log-initialize
```

This sets up the core system and loads essential modules.

### 2. Create Your First Memory Object

```elisp
M-x meta-log-r5rs-eval RET
```

Then evaluate:
```scheme
(substrate-create-memory 
  #u8(1 2 3 4 5)
  '((content-type . "test-data")
    (version . 1)))
```

You should see a result like:
```
(memory-object "mlss://sha3-256/abc123..." ...)
```

### 3. Run a Demo

The easiest way to see meta-log in action is to run the demos:

```bash
# Autonomy and Awareness Demo
./tests/demo-autonomy-awareness.sh

# Quick MLSS Demo
./tests/demo-mlss-quick.sh
```

You should see:
- ✅ Autonomous cycle working
- ✅ Self-monitoring operational
- ✅ Consciousness state updates
- ✅ All demos passing

## Common Use Cases

### Use Case 1: Create and Store Data

```scheme
;; Create memory object
(let* ((data #u8(1 2 3 4 5 6 7 8))
       (meta '((content-type . "my-data")))
       (result (substrate-create-memory data meta))
       (uri (list-ref result 1)))
  (display "Stored at: ")
  (display uri)
  (newline))
```

**What happens**: Data is content-addressed, gets a unique URI, and is stored in the substrate.

### Use Case 2: Transform Data

```scheme
;; Create CBS and transform
(let* ((cbs (make-cbs #u8(1 2 3 4) '((encoding . "raw"))))
       (transformed (binary-xor cbs #u8(255 0 255 0))))
  transformed)
```

**What happens**: Binary data is transformed (XOR operation), transformation is reversible, provenance is tracked.

### Use Case 3: Work with Waveforms

```scheme
;; Create waveform from samples
(let ((waveform (make-waveform '(0.5 0.7 0.9 0.7 0.5) '() 44100)))
  (display "Sample count: ")
  (display (length (waveform-get-samples waveform)))
  (newline))
```

**What happens**: Time-domain signal is created, can be analyzed in frequency domain, can be transformed to other layers.

### Use Case 4: Make Autonomous Decisions

```scheme
;; Create Q* state and evaluate actions
(let* ((state (make-qstar-state '((x . 0) (y . 0)) '((goal-x . 10) (goal-y . 10))))
       (action (make-qstar-action 'transform 'move-right '((dx . 1))))
       (result (qstar-evaluate state action))
       (cost (list-ref result 0)))
  (display "Action cost: ")
  (display cost)
  (newline))
```

**What happens**: System evaluates action, computes costs, selects optimal action, learns from outcomes.

### Use Case 5: Monitor Consciousness State

```scheme
;; Create conscious state and monitor
(let* ((state (make-conscious-state 5.0 0.7 0.8))
       (metrics (monitor-own-state state)))
  (display "Self-awareness: ")
  (display (assoc-ref metrics 'self-awareness-index))
  (newline))
```

**What happens**: Consciousness state is created, system monitors itself, detects anomalies, measures awareness.

## Next Steps

### Explore Core Concepts

- [Core Concepts](CORE-CONCEPTS) - Understand the fundamental ideas
- [What is Meta-Log?](WHAT-IS-META-LOG) - System overview
- [Autonomy & Awareness](AUTONOMY-AWARENESS) - How autonomy works

### Try Advanced Features

- [MLSS Guide](MLSS_GUIDE) - Complete technical guide
- [MLSS Use Cases](MLSS_USE_CASES) - Real-world examples
- [API Reference](API-REFERENCE) - Function documentation

### Learn the Architecture

- [Architecture](ARCHITECTURE) - System architecture overview
- [Modules](MODULES) - Component documentation
- [Status](STATUS) - Current implementation status

## Troubleshooting

### Issue: Guile not found

**Solution**: Install Guile 3.0+
```bash
# Ubuntu/Debian
sudo apt-get install guile-3.0

# macOS
brew install guile
```

### Issue: Python dependencies fail

**Solution**: Use virtual environment
```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

### Issue: Demos fail

**Solution**: Check that all services are running
```bash
# Start FastAPI services (if needed)
cd services/e8-api && python main.py &
cd services/vision-api && python main.py &
```

### Issue: Emacs can't find meta-log

**Solution**: Add to load-path
```elisp
(add-to-list 'load-path "/path/to/meta-log")
(require 'meta-log)
```

## Getting Help

- **Documentation**: Browse [all guides](index)
- **GitHub Issues**: Report bugs or ask questions
- **Examples**: Check `examples/` directory for more examples
- **Tests**: Run `./tests/demo-mlss-quick.sh` to verify installation

## What's Next?

Now that you're set up:

1. **Explore Concepts**: Read [Core Concepts](CORE-CONCEPTS) to understand the system
2. **Try Examples**: Run the demos and modify them
3. **Build Something**: Create your own use case
4. **Join Community**: Contribute back or ask questions

---

**Ready to dive deeper?** Check out [Core Concepts](CORE-CONCEPTS) or [MLSS Guide](MLSS_GUIDE) for technical details.

