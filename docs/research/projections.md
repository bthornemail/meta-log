---
layout: default
title: 3D Projections
nav_order: 5
description: "Projecting 2D CanvasL into 3D AR/VR space"
permalink: /research/projections
---

# 3D Projections

## The Projective Completion

The 2D affine space (CanvasL docs + Org/Obsidian canvas) projects natively into 3D computational space via WebGL/Three.js/A-Frame, with full AR/VR device support.

## Mathematical Mapping

From dual-pairs theory:

- **2D Affine Space** = Data layer (left adjoint: construction)
  - Docs/Org/CanvasL nodes = finite points (x,y)
  - Edges = GCD relations (what things ARE)

- **3D Projective Space** = Computational layer (right adjoint: observation)
  - Z=0 plane = affine embedding (2D data injects losslessly)
  - Z→∞ = projective completion (LCM behaviors, AR/VR infinity)
  - Media boundary = S⁷ (SVG=texture, WEBM=video, GLB=model, blobs=dynamic)

BQF discriminant Δ classifies the projection: definite (stable AR scenes) vs indefinite (VR evolutions).

## Implementation

### Three.js + A-Frame Integration

```typescript
class Metaverse3D {
  projectCanvasL(data: string) {
    const strata = data.split('\n').map(line => {
      try { return JSON.parse(line); } catch { return null; }
    }).filter(Boolean);

    strata.forEach((obj: any) => {
      if (obj.type === 'node') {
        const geometry = new THREE.SphereGeometry(1, 32, 32);
        const material = new THREE.MeshBasicMaterial({ color: obj.color || 0x00ff00 });
        const node = new THREE.Mesh(geometry, material);
        node.position.set(obj.x / 10, obj.y / 10, 0);  // Affine plane embed
        this.scene.add(node);
      }
    });
  }

  loadMedia() {
    // SVG as texture
    const textureLoader = new THREE.TextureLoader();
    textureLoader.load('example.svg', (texture) => {
      const material = new THREE.MeshBasicMaterial({ map: texture });
      const plane = new THREE.Mesh(new THREE.PlaneGeometry(5, 5), material);
      this.scene.add(plane);
    });

    // WEBM as video texture
    const video = document.createElement('video');
    video.src = 'example.webm';
    const videoTexture = new THREE.VideoTexture(video);
    const videoMaterial = new THREE.MeshBasicMaterial({ map: videoTexture });
    const videoPlane = new THREE.Mesh(new THREE.PlaneGeometry(16, 9), videoMaterial);
    this.scene.add(videoPlane);

    // GLB as 3D model
    const loader = new GLTFLoader();
    loader.load('example.glb', (gltf) => {
      this.scene.add(gltf.scene);
    });
  }
}
```

## R5RS Bridge

Project from Emacs/Org:

```scheme
(define (project-2d-to-3d canvasl-file)
  ;; Read 2D affine CanvasL
  (let ((strata (jsonl-parse-file canvasl-file)))
    ;; Project to 3D: add z=0, S7 media boundary
    (for-each (lambda (obj)
                (if (eq? (assoc-ref obj 'type) 'node)
                    (assoc-set! obj 'position `(x ,(assoc-ref obj 'x)
                                                y ,(assoc-ref obj 'y)
                                                z 0))))  ; Affine embed
              strata)
    ;; Add media S7
    (append strata '((type . media_boundary)
                     (S7_infinity . ((svg . texture)
                                     (webm . video)
                                     (glb . model)
                                     (blob . dynamic)))))
    ;; Export to TS/Three.js JSON
    (jsonl-write-file "metaverse-3d.json" strata)))
```

## AR/VR Experience

When you open the 3D scene in AR/VR:
- CanvasL nodes appear as floating holographic spheres
- Drag them → physics updates → consensus recomputed
- Tap a node → holographic button with M-expression label
- GLB models float in space
- WEBM videos play on planes
- Blob spheres show live JSON law
- All changes sync back to Emacs via MQTT → Org → CanvasL → 2D Obsidian

## The Duality

| Layer | Engine | Duality Role |
|-------|--------|--------------|
| 2D Affine (data) | Obsidian Canvas / Org | Left adjoint – construction |
| 3D Projective (law) | Three.js / Babylon.js | Right adjoint – observation |
| S⁷ at infinity | GLB/WEBM/SVG/blobs | Media boundary of consciousness |

## References

- [3D Computational Manifolds](../3D_COMPUTATIONAL_MANIFOLDS.md) - Complete 3D framework
- [Babylon Integration](babylon-integration.md) - Babylon.js engine
- [Research: 10-projections](../dev-docs/research/10-projections.md) - Original projections document

