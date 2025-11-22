---
layout: default
title: CanvasL Dual-Pair Stratification
nav_order: 2
description: "The fundamental rule for organizing CanvasL files using dual-pair stratification"
permalink: /research/canvasl-strata
---

# CanvasL Dual-Pair Stratification

## The Fundamental Rule

CanvasL files must follow **dual-pair stratification** - a geometric law that organizes computational structures into two complementary sides.

## The One True Restructuring Rule

**NEVER** mix left and right sides in the same file. Each CanvasL file must be pure:

```typescript
// NEVER do this (evil, destroys geometry):
{
  "nodes": [...],
  "edges": [...]
}

// ALWAYS keep this exact dual-pair stratification:
<file1>.canvasl   → pure left side  (affine / GCD / what things ARE)
<file2>.canvasl   → pure right side (projective / LCM / what things DO)
```

## Left Side (Affine/GCD)

**What things ARE**:
- Pure value layer
- Ports, 8-tuple, Environment
- Wallet → Address → Blockchain
- Static structure
- Data, ontology

**Example Files**:
- `root.json` - Ports, 8-tuple, Environment (pure affine)
- `token.json` - Wallet→Address→Blockchain (pure value layer)
- `categories.canvasl` - Economic ontology (what categories exist)

## Right Side (Projective/LCM)

**What things DO**:
- Pure action layer
- Consumer ↔ Provider
- Order ↔ Fulfillment
- Publish/Subscribe/Chat/Trade
- Behavior, rules, procedures

**Example Files**:
- `service.json` - Consumer↔Provider, Order↔Fulfillment (pure actions)
- `marketplace.json` - Publish/Subscribe/Chat/Trade (pure transformations)
- `university.json` - Scholar→Exam→Achievement (pure procedures)

## Why This Matters

Dual-pair stratification:
1. **Preserves Geometry**: Maintains mathematical structure
2. **Enables Verification**: Allows geometric proof checking
3. **Prevents Errors**: Catches structural mistakes automatically
4. **Enables Composition**: Files can be safely combined

## File Format Rules

### Use `.canvasl` Extension

**ALWAYS** use `.canvasl` for:
- Pure left-side / affine / GCD files
- Pure right-side / projective / LCM files
- Literate mathematical law files
- ANY file that will be `@include`-d into topology/system

**NEVER** use:
- `.yaml` - Not part of the 8-tuple → destroys affine purity
- `.json` - No `@include`, no directives → cannot participate in the sheaf
- `.jsonl` - Only for logs (no CanvasL directives)

### Example CanvasL File

```jsonl
@version 1.0
@schema economic-ontology-v1
@dimension 0D
@branch 01-ontology
@role "Human Economic Activity Spanning Set"

{"id":"category/Home Services", "type":"economic-category", "subcategories":[
  "General Handyman","Plumbing","Electrical","Roofing","HVAC (Heating & Cooling)",
  "Home Cleaning","Deep Cleaning","Move-in/Move-out Cleaning","Carpet Cleaning",
  "Window Cleaning","Lawn Care","Landscaping","Gutter Cleaning"
]}

{"id":"category/Auto & Mechanical Services", "type":"economic-category", "subcategories":[
  "General Auto Repair","Mobile Mechanic","Brake Services","Tire Services"
]}

{"id":"economic-ontology-complete", "type":"gcd", "spans":"all-human-labor", "uri":"canvasl://categories/2025-v1"}
```

## Directory Structure

The metaverse requires this structure:

```
metaverse/
├── shape.canvasl         ← 8D affine space + S7 at infinity + Schläfli/Betti metadata
├── centroid.canvasl      ← statistical center + anomaly detection
├── topology.canvasl      ← left bipartition (= all left-side files merged as @include)
├── system.canvasl        ← right bipartition (= all right-side files merged as @include)
└── automaton.canvasl     ← unified self-reference (the living being)
```

### Topology File (Left Side)

```jsonl
// metaverse/topology.canvasl  (left-side / affine)
@include "../root.json"
@include "../token.json"
@include "../categories.canvasl"
@include "../dual-pairs-unified.md"
```

### System File (Right Side)

```jsonl
// metaverse/system.canvasl  (right-side / projective)
@include "../service.json"
@include "../marketplace.json"
@include "../university.json"
```

## The Law

**Preserve the duality. Preserve the asymmetry.**

The cube is watching.

## References

- [Dual Pairs](../concepts/dual-pairs.md) - Complete dual-pair theory
- [Research: 01-rules](../dev-docs/research/01-rules.md) - Original rules document

