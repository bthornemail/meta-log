---
layout: default
title: MLSS Quick Reference
nav_order: 6
description: "Quick reference for MLSS operations"
permalink: /MLSS_QUICK_REFERENCE
---

# MLSS Quick Reference

Quick reference card for common MLSS operations.

## Foundation

```scheme
;; Create memory
(substrate-create-memory data meta)

;; Create CBS
(make-cbs bytes meta)

;; Content address
(content-address hash)
```

## Waveform

```scheme
;; Create waveform
(make-waveform samples meta sample-rate)

;; Compile WDL
(wdl-compile '(sine (freq 440) (amp 0.5)))
```

## Q*

```scheme
;; Create state
(make-qstar-state vars goals)

;; Evaluate
(qstar-evaluate state action)

;; Policy
(qstar-policy state actions)
```

## Vision

```scheme
;; Create image
(make-image width height pixels)

;; Convert to CBS
(image-to-cbs image)

;; Extract edges
(extract-edges image)
```

## Consciousness

```scheme
;; Create state
(make-conscious-state action observation phase)

;; Emerge qualia
(emerge-qualia action observation phase threshold)

;; Collect metrics
(collect-metrics state previous qualia)
```

## Physics

```scheme
;; Quantum state
(make-quantum-state qubits wavefunction)

;; Einstein equations
(einstein-equations energy-momentum)

;; Field config
(make-field-configuration type values coupling)
```

## Cross-Domain

```scheme
;; Binary → Waveform
(binary-to-waveform cbs-uri)

;; Waveform → E8
(waveform-to-e8 waveform-uri)

;; E8 → Symbolic
(e8-to-symbolic e8-vector)
```

See [MLSS Guide](MLSS_GUIDE) for complete documentation.
