# MLSS Quick Start Guide

## Loading the Substrate System

### From Emacs

```elisp
(require 'meta-log)
(require 'meta-log-substrate-runtime)
(meta-log-substrate-initialize)
```

### From Scheme (via Geiser)

```scheme
(load "scheme/r5rs-canvas-engine.scm")
```

## Basic Operations

### Create Memory Object

```scheme
(substrate-create-memory #u8(1 2 3 4) '((encoding . "raw")))
;; Returns: (memory-object uri)
```

### Transform Binary

```scheme
(substrate-transform "mlss://sha3-256/..." "xor" '((mask . #u8(255 0))))
;; Returns: (cbs-object uri)
```

### Get Provenance

```scheme
(substrate-get-provenance "mlss://sha3-256/...")
;; Returns: list of provenance records
```

### Q* Evaluation

```scheme
(qstar-evaluate-action state-id action)
;; Returns: (q-value plan provenance)
```

### Create Waveform

```scheme
(waveform-create samples 48000 1.0 1)
;; Returns: (waveform-object uri)
```

### Cross-Domain Mapping

```scheme
(binary-to-waveform cbs-id 'direct '((sample-rate . 48000)))
(waveform-to-e8 waveform-id 'spectral '())
(e8-to-symbolic e8-id 'threshold '((thresholds . (0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8))))
```

## Content Addressing

All substrate objects are content-addressed:

```scheme
"mlss://sha3-256/abc123..."
```

Resolve URIs:

```scheme
(substrate-resolve-uri "mlss://sha3-256/...")
```

## FastAPI Services

Heavy operations use FastAPI (if needed):

```bash
# Hash computation
curl -X POST http://localhost:8001/api/v1/substrate/hash \
  -H "Content-Type: application/json" \
  -d '{"data": "AQIDBA==", "algorithm": "sha3-256"}'
```

## See Also

- `dev-docs/INTEGRATION-MLSS.md` - Full integration specification
- `dev-docs/MLSS-IMPLEMENTATION-SUMMARY.md` - Implementation details
- `scheme/README.md` - Module documentation

