---
layout: default
title: Rendering and Visualization
nav_order: 4
description: "Visual programming language for the 8D metaverse"
permalink: /research/rendering
---

# Rendering and Visualization

## The Visual Programming Format

CanvasL is the native visual programming language of the 8D metaverse. It is simultaneously:
- Executable R5RS Scheme
- Content-addressable geometric stratum
- Native Obsidian Canvas / JSON Canvas superset
- Live Mermaid graph
- Avatar + icon + spatial layout aware
- Dual-pair stratified (affine left / projective right)

## Visual Program Structure

A CanvasL visual program combines:
1. **Graph Structure**: Nodes, edges, subgraphs
2. **Spatial Layout**: X/Y coordinates for visual arrangement
3. **Visual Metadata**: Icons, colors, labels
4. **Executable Code**: R5RS Scheme functions
5. **Geometric Metadata**: Schläfli symbols, Betti numbers

## Example Visual Program

```jsonl
@version 1.0
@schema canvasl-visual-program-v1
@dimension 4D
@branch 02-syntax
@visual obsidian-canvas
@render mermaid + avatars + icons
@title "Complete Economic Flow Metaverse – Visual Program"

{"id":"use-cases", "type":"subgraph", "label":"Use Cases", "layout":"cluster", "icon":"👥"}
{"id":"nfts", "type":"subgraph", "label":"NFT Types", "layout":"cluster", "icon":"🖼️"}
{"id":"forms", "type":"subgraph", "label":"Live Forms", "layout":"cluster", "icon":"📋"}

{"id":"provideUsageRightsOfAssetToCommunityOfChoice", "parent":"use-cases", "label":"Lease Equipment", "icon":"🔧", "x":100, "y":200}
{"id":"LIFE_TOKEN", "parent":"nfts", "label":"LIFE_TOKEN", "icon":"💚", "color":"#00ff00"}
{"id":"Asset_Lease_Form", "parent":"forms", "label":"Lease Equipment Form", "icon":"📝", "color":"#4adeff"}

{"edge":"LIFE_TOKEN --> Asset_Lease_Form", "label":"pays for"}

{"id":"executable", "type":"r5rs-call", "function":"metaverse.deployEconomicFlows", "args":["#use-cases","#nfts","#forms"]}
```

## Rendering Engines

### Obsidian Canvas
- Native spatial canvas rendering
- Drag-and-drop avatars
- Live graph updates
- Icon and color support

### Mermaid
- Live graph rendering
- Automatic layout
- Interactive nodes
- Export to SVG/PNG

### Browser/WebGL
- 3D visualization (see [Projections](projections.md))
- Interactive manipulation
- Real-time updates
- AR/VR support

## What Happens When You Open a CanvasL File

1. **Obsidian**: Instantly renders as spatial canvas with avatars and icons
2. **Mermaid**: Live-renders the graph structure
3. **Clicking Nodes**: Opens actual live forms (IDs match CanvasL objects)
4. **Executing Code**: Final line executes the entire system when evaluated
5. **Content Addressing**: Everything is addressable via Schläfli symbol
6. **Drag Avatars**: Execution topology changes in real time

## The Breakthrough

CanvasL files are now:
- **Geometrically-native**: Live inside the 8-dimensional computational manifold
- **Avatar-based**: Visual representation with icons and spatial layout
- **Live-executable**: R5RS Scheme code runs directly
- **Multi-format**: Works in Obsidian, browser, and on-chain

You don't need:
- JSON Canvas (CanvasL is the superset)
- Mermaid (CanvasL includes it)
- Obsidian plugins (CanvasL is the plugin)

**You ARE the plugin.**

## References

- [Projections](projections.md) - 2D to 3D projection
- [Babylon Integration](babylon-integration.md) - 3D rendering engine
- [Research: 03-rendering](../dev-docs/research/03-rendering.md) - Original rendering document

