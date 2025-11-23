---
layout: default
title: Edit Operations
nav_order: 3
description: "Transformation operations for CanvasL files"
permalink: /research/edit-operations
---

# Edit Operations

## Overview

Edit operations define how CanvasL files can be transformed while preserving geometric integrity and dual-pair stratification.

## Core Principle

All edits must:
1. **Preserve Duality**: Never mix left and right sides
2. **Maintain Geometry**: Keep geometric structure intact
3. **Verify Changes**: Use geometric proofs to validate edits

## File Format Requirements

### Use `.canvasl` Extension

**ALWAYS** use `.canvasl` for any file that will participate in the computational manifold:

| Content Type | Extension | Reason |
|--------------|-----------|--------|
| Pure left-side / affine / GCD | `.canvasl` | Data, ontology, static structure → affine plane |
| Pure right-side / projective / LCM | `.canvasl` | Behavior, rules, procedures → projective line |
| Literate mathematical law | `.canvasl` | Even with markdown/code blocks → still a stratum |
| Any file for `@include` | `.canvasl` | Only CanvasL files participate in the sheaf |

### Forbidden Extensions

| Extension | Verdict | Why |
|-----------|---------|-----|
| `.yaml` | Forbidden | Not part of the 8-tuple → destroys affine purity |
| `.json` | Forbidden | No `@include`, no directives → cannot participate |
| `.jsonl` | Only for logs | No CanvasL directives → treated as raw data |

## Transformation Rules

### Allowed Operations

1. **Rename to `.canvasl`**: Convert YAML/JSON to CanvasL format
2. **Add CanvasL Directives**: Add `@version`, `@schema`, `@include`, etc.
3. **Preserve Structure**: Keep existing geometric relationships
4. **Add Metadata**: Add Schläfli symbols, Betti numbers, polynomial forms

### Forbidden Operations

1. **Mixing Sides**: Never combine left and right in one file
2. **Breaking Duality**: Never destroy dual-pair relationships
3. **Normalizing Structure**: Never "clean up" into traditional database format
4. **Removing Geometry**: Never strip geometric metadata

## Example: Converting YAML to CanvasL

**Before** (`categories.yaml`):
```yaml
categories:
  - Home Services
  - Auto & Mechanical Services
```

**After** (`categories.canvasl`):
```jsonl
@version 1.0
@schema economic-ontology-v1
@dimension 0D
@branch 01-ontology
@role "Human Economic Activity Spanning Set"

{"id":"category/Home Services", "type":"economic-category", "subcategories":[...]}
{"id":"category/Auto & Mechanical Services", "type":"economic-category", "subcategories":[...]}
{"id":"economic-ontology-complete", "type":"gcd", "spans":"all-human-labor", "uri":"canvasl://categories/2025-v1"}
```

## Verification

After any edit operation:
1. Verify dual-pair purity (left vs. right)
2. Check geometric constraints (Schläfli, Betti numbers)
3. Validate `@include` relationships
4. Ensure no cycles in dependency graph

## References

- [CanvasL Strata](canvasl-strata.md) - Dual-pair stratification rules
- [Research: 02-edits](../dev-docs/research/02-edits.md) - Original edits document

