# 3D Template Manipulation and Video Generation Framework

**Template-driven 3D asset manipulation with procedural generation, WebGL rendering, and temporal video synthesis** for the Meta-Log Substrate System.

## Abstract

The Meta-Log Substrate System's CanvasL metaverse layer provides 2D→3D projective transformation of graph data into AR/VR space. This document extends that framework with **template-based 3D asset manipulation**, **procedural geometry generation**, and **temporal frame sequencing for video output**—completing the visual synthesis pipeline from symbolic data to rendered video.

The core insight: **3D manipulation is a categorical right adjoint to 2D template construction**. CanvasL templates (left adjoint) freely construct graph data; 3D renderers (right adjoint) observe and evaluate that data as spatial geometry. Video generation extends this temporally: frame sequences (left adjoint) construct temporal evolution; video encoding (right adjoint) observes and compresses that evolution into playable media.

## The Template ⊣ Manipulation Adjunction

### 1. Template as Free Construction (Left Adjoint)

**CanvasL templates** are JSONL specifications of visual structure:

```jsonl
{"id":"quantum-field","type":"node","x":100,"y":200,"geometry":"sphere","scale":2.0,"color":"#00ff00"}
{"id":"gr-spacetime","type":"node","x":300,"y":150,"geometry":"torus","scale":1.5,"color":"#ff0000"}
{"id":"entangle","from":"quantum-field","to":"gr-spacetime","type":"edge","visualize":"particle-flow"}
{"id":"physics-sim","type":"model","file":"heavy-equipment.glb","position":[0,0,-5],"scale":1.0}
```

**Properties of templates (as free objects)**:
- **Compositional**: Templates combine via set union
- **Serializable**: Pure data, no side effects
- **Version-controlled**: Content-addressed via CBS (Canonical Binary Substrate)
- **Human-editable**: Text-based specification
- **Domain-agnostic**: Same template language for any 3D content

Templates are **initial algebras** in the category of visual specifications. The functor `F X = Node + Edge + Model + X` describes template formation; templates are the least fixed point `μF`.

### 2. 3D Manipulation as Observation (Right Adjoint)

**WebGL renderers** evaluate templates into spatial geometry:

```typescript
class Template3DRenderer {
  scene: THREE.Scene;
  objects: Map<string, THREE.Object3D>;

  // The right adjoint: template → observable 3D scene
  evaluate(template: CanvasLTemplate): THREE.Scene {
    template.forEach(spec => {
      switch(spec.type) {
        case 'node':
          this.createProceduralGeometry(spec);
          break;
        case 'model':
          this.loadGLBAsset(spec);
          break;
        case 'edge':
          this.createConnection(spec);
          break;
      }
    });
    return this.scene;
  }

  // Procedural geometry factory
  createProceduralGeometry(spec: NodeSpec): THREE.Object3D {
    const generators = {
      'sphere': () => new THREE.SphereGeometry(spec.scale, 32, 32),
      'box': () => new THREE.BoxGeometry(spec.scale, spec.scale, spec.scale),
      'torus': () => new THREE.TorusGeometry(spec.scale, spec.scale * 0.4, 16, 100),
      'icosahedron': () => new THREE.IcosahedronGeometry(spec.scale, 1),
      'parametric': () => this.generateParametricSurface(spec)
    };

    const geometry = generators[spec.geometry]();
    const material = new THREE.MeshStandardMaterial({
      color: spec.color,
      metalness: spec.metalness || 0.5,
      roughness: spec.roughness || 0.5
    });
    const mesh = new THREE.Mesh(geometry, material);

    // Apply E8 → 3D projection
    mesh.position.set(spec.x / 50, spec.y / 50, spec.z || 0);

    this.objects.set(spec.id, mesh);
    this.scene.add(mesh);
    return mesh;
  }

  // GLB asset loading and manipulation
  loadGLBAsset(spec: ModelSpec): Promise<THREE.Object3D> {
    const loader = new GLTFLoader();
    return new Promise((resolve) => {
      loader.load(spec.file, (gltf) => {
        const model = gltf.scene;

        // Apply template transformations
        if (spec.position) model.position.set(...spec.position);
        if (spec.rotation) model.rotation.set(...spec.rotation);
        if (spec.scale) model.scale.setScalar(spec.scale);

        // Material override from template
        if (spec.material) {
          model.traverse((child) => {
            if (child instanceof THREE.Mesh) {
              child.material = this.createMaterial(spec.material);
            }
          });
        }

        this.objects.set(spec.id, model);
        this.scene.add(model);
        resolve(model);
      });
    });
  }
}
```

**Properties of 3D manipulation (as terminal objects)**:
- **Observable**: Renders to pixels (screen/texture/framebuffer)
- **Physical**: Participates in physics simulation (Havok)
- **Interactive**: Responds to user input (raycasting, hand tracking)
- **Temporal**: Animates over time (particle flows, rotations)

### 3. The Adjunction Unit and Counit

**Unit** `η: Template → Render(Template)`: Embedding template data into 3D space
```typescript
// 2D template coordinates → 3D affine embedding
const embed2Dto3D = (x: number, y: number): THREE.Vector3 =>
  new THREE.Vector3(x / 50, y / 50, 0);  // Z=0 plane
```

**Counit** `ε: Template(Render(Scene)) → Scene`: Extracting scene from rendered state
```typescript
// Scene snapshot → Template reconstruction
const extractTemplate = (scene: THREE.Scene): CanvasLTemplate => {
  const template = [];
  scene.traverse((obj) => {
    if (obj.userData.templateId) {
      template.push({
        id: obj.userData.templateId,
        type: 'node',
        position: obj.position.toArray(),
        rotation: obj.rotation.toArray(),
        scale: obj.scale.x
      });
    }
  });
  return template;
};
```

**Adjunction law**: `ε ∘ Render(η) = id` (evaluating an embedded template yields the original template structure)

## GLB/GLTF Manipulation Pipeline

### Asset Transformation System

```typescript
interface GLBManipulator {
  // Load and parse GLB
  load(path: string): Promise<GLTF>;

  // Transform operations
  transform(asset: GLTF, ops: TransformOps): GLTF;

  // Material operations
  recolor(asset: GLTF, colorMap: Record<string, string>): GLTF;
  retexture(asset: GLTF, textureMap: Record<string, Texture>): GLTF;

  // Geometry operations
  subdivide(mesh: THREE.Mesh, level: number): THREE.Mesh;
  simplify(mesh: THREE.Mesh, targetRatio: number): THREE.Mesh;

  // Animation operations
  extractAnimation(asset: GLTF, clipName: string): THREE.AnimationClip;
  blendAnimations(clips: THREE.AnimationClip[], weights: number[]): THREE.AnimationClip;

  // Procedural modifications
  applyShader(mesh: THREE.Mesh, shader: ShaderMaterial): void;
  generateLODs(mesh: THREE.Mesh, levels: number[]): THREE.LOD;

  // Export
  export(asset: GLTF): ArrayBuffer;
}

// Example: Template-driven GLB manipulation
const manipulateGLB = async (template: ModelTemplate) => {
  const asset = await glbManipulator.load(template.file);

  // Apply template-specified transformations
  if (template.recolor) {
    glbManipulator.recolor(asset, template.recolor);
  }

  if (template.animations) {
    template.animations.forEach(anim => {
      const clip = glbManipulator.extractAnimation(asset, anim.clip);
      // Modify animation timing, blending, etc.
    });
  }

  if (template.procedural) {
    asset.scene.traverse((mesh) => {
      if (mesh instanceof THREE.Mesh && template.procedural.shader) {
        glbManipulator.applyShader(mesh, createShader(template.procedural.shader));
      }
    });
  }

  return asset;
};
```

### Babylon.js Integration (Canonical Engine)

As specified in `docs/research/babylon-integration.md`, **Babylon.js is the one true 3D engine**:

```typescript
import { SceneLoader, MeshBuilder, StandardMaterial, PhysicsImpostor } from "@babylonjs/core";

class BabylonTemplateRenderer {
  async renderTemplate(template: CanvasLTemplate): Promise<BABYLON.Scene> {
    const scene = new BABYLON.Scene(this.engine);

    for (const spec of template) {
      if (spec.type === 'node') {
        const mesh = this.createProceduralMesh(spec, scene);

        // Physics from template
        if (spec.physics) {
          mesh.physicsImpostor = new PhysicsImpostor(
            mesh,
            PhysicsImpostor.SphereImpostor,
            { mass: spec.physics.mass || 1 },
            scene
          );
        }

        // Holographic GUI labels
        if (spec.label) {
          this.attachHolographicLabel(mesh, spec.label, scene);
        }
      } else if (spec.type === 'model') {
        // Import GLB with transformations
        const result = await SceneLoader.ImportMeshAsync("", "/", spec.file, scene);
        result.meshes.forEach(mesh => {
          if (spec.position) mesh.position = BABYLON.Vector3.FromArray(spec.position);
          if (spec.scale) mesh.scaling = new BABYLON.Vector3(spec.scale, spec.scale, spec.scale);
        });
      }
    }

    return scene;
  }

  createProceduralMesh(spec: NodeSpec, scene: BABYLON.Scene): BABYLON.Mesh {
    const factories = {
      sphere: () => MeshBuilder.CreateSphere(spec.id, { diameter: spec.scale }, scene),
      box: () => MeshBuilder.CreateBox(spec.id, { size: spec.scale }, scene),
      torus: () => MeshBuilder.CreateTorus(spec.id, { diameter: spec.scale, thickness: spec.scale * 0.3 }, scene),
      cylinder: () => MeshBuilder.CreateCylinder(spec.id, { diameter: spec.scale, height: spec.scale * 2 }, scene)
    };

    const mesh = factories[spec.geometry]();

    const material = new StandardMaterial(`mat-${spec.id}`, scene);
    material.emissiveColor = BABYLON.Color3.FromHexString(spec.color || "#ffffff");
    mesh.material = material;

    return mesh;
  }
}
```

## Parametric and Procedural Generation

### Mathematical Surface Generation

Leverage the p-adic curves and E8 geometry from existing substrate:

```typescript
interface ParametricSpec {
  type: 'parametric';
  equation: 'trefoil' | 'klein' | 'mobius' | 'epicycloid' | 'e8-root-projection';
  parameters?: Record<string, number>;
  uRange?: [number, number];
  vRange?: [number, number];
  resolution?: number;
}

class ProceduralGeometry {
  // Trefoil knot (from p-adic curves research)
  generateTrefoil(R: number = 2, r: number = 1): THREE.BufferGeometry {
    const curve = new THREE.Curve<THREE.Vector3>();
    curve.getPoint = (t: number): THREE.Vector3 => {
      const theta = t * Math.PI * 2;
      return new THREE.Vector3(
        (R + r * Math.cos(3 * theta)) * Math.cos(2 * theta),
        (R + r * Math.cos(3 * theta)) * Math.sin(2 * theta),
        r * Math.sin(3 * theta)
      );
    };

    return new THREE.TubeGeometry(curve, 100, 0.3, 8, true);
  }

  // Klein bottle
  generateKlein(scale: number = 1): THREE.BufferGeometry {
    const vertices = [];
    const uRes = 50, vRes = 50;

    for (let u = 0; u < uRes; u++) {
      for (let v = 0; v < vRes; v++) {
        const theta = (u / uRes) * Math.PI * 2;
        const phi = (v / vRes) * Math.PI * 2;

        const x = scale * (Math.cos(theta) * (Math.cos(theta/2) * (Math.sqrt(2) + Math.cos(phi)) + Math.sin(theta/2) * Math.sin(phi) * Math.cos(phi)));
        const y = scale * (Math.sin(theta) * (Math.cos(theta/2) * (Math.sqrt(2) + Math.cos(phi)) + Math.sin(theta/2) * Math.sin(phi) * Math.cos(phi)));
        const z = scale * (-Math.sin(theta/2) * (Math.sqrt(2) + Math.cos(phi)) + Math.cos(theta/2) * Math.sin(phi) * Math.cos(phi));

        vertices.push(x, y, z);
      }
    }

    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute('position', new THREE.Float32BufferAttribute(vertices, 3));
    geometry.computeVertexNormals();
    return geometry;
  }

  // E8 root lattice projection (240 points → 3D)
  generateE8Projection(scale: number = 1): THREE.BufferGeometry {
    // E8 has 240 roots; project to R³ via stereographic projection
    const roots = this.computeE8Roots();  // From scheme/substrate/e8.scm
    const vertices = [];

    roots.forEach(root => {
      // Stereographic projection from R⁸ → R³
      const projected = this.stereographicProject(root);
      vertices.push(projected.x * scale, projected.y * scale, projected.z * scale);
    });

    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute('position', new THREE.Float32BufferAttribute(vertices, 3));
    return geometry;
  }

  // Epicycloid from p-adic curves research
  generateEpicycloid(R: number, r: number, samples: number = 200): THREE.Curve<THREE.Vector3> {
    return new THREE.Curve<THREE.Vector3>({
      getPoint(t: number): THREE.Vector3 {
        const theta = t * Math.PI * 2;
        return new THREE.Vector3(
          (R + r) * Math.cos(theta) - r * Math.cos((R + r) / r * theta),
          (R + r) * Math.sin(theta) - r * Math.sin((R + r) / r * theta),
          0
        );
      }
    });
  }
}
```

### Template-Driven Procedural Generation

```jsonl
{"id":"trefoil-knot","type":"parametric","equation":"trefoil","R":2,"r":1,"color":"#00ffff"}
{"id":"klein-bottle","type":"parametric","equation":"klein","scale":1.5,"color":"#ff00ff"}
{"id":"e8-lattice","type":"parametric","equation":"e8-root-projection","scale":2.0,"color":"#ffff00"}
{"id":"physics-field","type":"quantum-field","wavefunction":"psi-entangled","scale":3.0}
```

Renderer processes these via parametric geometry factory:

```typescript
if (spec.type === 'parametric') {
  const geometry = proceduralGen.generate(spec.equation, spec);
  const mesh = new THREE.Mesh(
    geometry,
    new THREE.MeshStandardMaterial({ color: spec.color, wireframe: true })
  );
  scene.add(mesh);
}
```

## Video Generation Pipeline

### Temporal Frame Sequencing

**Video = temporal sequence of 3D states**. Extend templates with time dimension:

```typescript
interface TemporalTemplate {
  duration: number;  // seconds
  fps: number;       // frames per second
  timeline: KeyFrame[];
}

interface KeyFrame {
  time: number;  // seconds
  objects: Record<string, ObjectState>;
}

interface ObjectState {
  position?: [number, number, number];
  rotation?: [number, number, number];
  scale?: number;
  color?: string;
  visible?: boolean;
}

// Example: Consciousness state evolution video
const consciousnessEvolution: TemporalTemplate = {
  duration: 10,
  fps: 30,
  timeline: [
    {
      time: 0,
      objects: {
        "qualia-field": { position: [0, 0, 0], scale: 1.0, color: "#ff0000" },
        "observation": { position: [5, 0, 0], scale: 0.5, color: "#00ff00" }
      }
    },
    {
      time: 5,
      objects: {
        "qualia-field": { position: [0, 2, 0], scale: 2.0, color: "#ff00ff" },
        "observation": { position: [5, 2, 0], scale: 1.5, color: "#00ffff" }
      }
    },
    {
      time: 10,
      objects: {
        "qualia-field": { position: [0, 5, 0], scale: 1.0, color: "#ffff00" },
        "observation": { position: [5, 5, 0], scale: 1.0, color: "#ffffff" }
      }
    }
  ]
};
```

### Frame Capture System

```typescript
class FrameCaptureRenderer {
  renderer: THREE.WebGLRenderer;
  scene: THREE.Scene;
  camera: THREE.Camera;

  captureFrame(): ImageData {
    this.renderer.render(this.scene, this.camera);

    // Read pixels from WebGL framebuffer
    const width = this.renderer.domElement.width;
    const height = this.renderer.domElement.height;
    const pixels = new Uint8Array(width * height * 4);

    const gl = this.renderer.getContext();
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);

    return new ImageData(new Uint8ClampedArray(pixels), width, height);
  }

  async renderSequence(template: TemporalTemplate): Promise<ImageData[]> {
    const frames: ImageData[] = [];
    const totalFrames = template.duration * template.fps;

    for (let frame = 0; frame < totalFrames; frame++) {
      const time = frame / template.fps;

      // Interpolate object states at this time
      const state = this.interpolateState(template.timeline, time);
      this.applyState(state);

      // Capture frame
      frames.push(this.captureFrame());

      // Progress callback
      console.log(`Rendered frame ${frame + 1}/${totalFrames}`);
    }

    return frames;
  }

  interpolateState(timeline: KeyFrame[], time: number): Record<string, ObjectState> {
    // Find surrounding keyframes
    let before = timeline[0];
    let after = timeline[timeline.length - 1];

    for (let i = 0; i < timeline.length - 1; i++) {
      if (timeline[i].time <= time && timeline[i + 1].time >= time) {
        before = timeline[i];
        after = timeline[i + 1];
        break;
      }
    }

    // Linear interpolation between keyframes
    const t = (time - before.time) / (after.time - before.time);
    const interpolated: Record<string, ObjectState> = {};

    const allObjects = new Set([...Object.keys(before.objects), ...Object.keys(after.objects)]);

    allObjects.forEach(objId => {
      const stateBefore = before.objects[objId] || {};
      const stateAfter = after.objects[objId] || stateBefore;

      interpolated[objId] = {
        position: this.lerpVector(stateBefore.position, stateAfter.position, t),
        rotation: this.lerpVector(stateBefore.rotation, stateAfter.rotation, t),
        scale: this.lerp(stateBefore.scale || 1, stateAfter.scale || 1, t),
        color: stateAfter.color  // No interpolation for colors (could add)
      };
    });

    return interpolated;
  }

  lerp(a: number, b: number, t: number): number {
    return a + (b - a) * t;
  }

  lerpVector(a?: number[], b?: number[], t?: number): [number, number, number] | undefined {
    if (!a || !b) return a as [number, number, number] || b as [number, number, number];
    return [
      this.lerp(a[0], b[0], t),
      this.lerp(a[1], b[1], t),
      this.lerp(a[2], b[2], t)
    ];
  }
}
```

### Video Encoding via FFmpeg

```typescript
import { spawn } from 'child_process';
import { createWriteStream } from 'fs';

class VideoEncoder {
  async encodeFrames(frames: ImageData[], options: VideoOptions): Promise<Buffer> {
    const { width, height, fps = 30, codec = 'libx264', format = 'mp4' } = options;

    // FFmpeg process: raw RGBA frames → encoded video
    const ffmpeg = spawn('ffmpeg', [
      '-f', 'rawvideo',
      '-pixel_format', 'rgba',
      '-video_size', `${width}x${height}`,
      '-framerate', fps.toString(),
      '-i', 'pipe:0',  // Read from stdin
      '-c:v', codec,
      '-pix_fmt', 'yuv420p',
      '-preset', 'fast',
      '-crf', '18',
      '-f', format,
      'pipe:1'  // Write to stdout
    ]);

    // Feed frames to FFmpeg
    frames.forEach(frame => {
      ffmpeg.stdin.write(Buffer.from(frame.data));
    });
    ffmpeg.stdin.end();

    // Collect output
    const chunks: Buffer[] = [];
    return new Promise((resolve, reject) => {
      ffmpeg.stdout.on('data', chunk => chunks.push(chunk));
      ffmpeg.stderr.on('data', data => console.log(`FFmpeg: ${data}`));
      ffmpeg.on('close', code => {
        if (code === 0) {
          resolve(Buffer.concat(chunks));
        } else {
          reject(new Error(`FFmpeg exited with code ${code}`));
        }
      });
    });
  }
}

interface VideoOptions {
  width: number;
  height: number;
  fps?: number;
  codec?: 'libx264' | 'libvpx-vp9' | 'libaom-av1';
  format?: 'mp4' | 'webm' | 'avi';
}
```

### Complete Video Generation Workflow

```typescript
class MetaLogVideoGenerator {
  async generateVideo(template: TemporalTemplate, outputPath: string): Promise<void> {
    console.log('Initializing 3D scene...');
    const renderer = new FrameCaptureRenderer(1920, 1080);

    // Load initial template state
    await renderer.loadTemplate(template.timeline[0].objects);

    console.log(`Rendering ${template.duration}s at ${template.fps}fps...`);
    const frames = await renderer.renderSequence(template);

    console.log('Encoding video...');
    const encoder = new VideoEncoder();
    const videoBuffer = await encoder.encodeFrames(frames, {
      width: 1920,
      height: 1080,
      fps: template.fps,
      codec: 'libx264',
      format: 'mp4'
    });

    // Write to file
    const fs = require('fs').promises;
    await fs.writeFile(outputPath, videoBuffer);

    console.log(`Video saved to ${outputPath}`);
  }
}

// Usage: Generate consciousness emergence video
const generator = new MetaLogVideoGenerator();
await generator.generateVideo(consciousnessEvolution, 'output/consciousness-emergence.mp4');
```

## Integration with Existing MLSS Components

### Quantum State Visualization

Render quantum states from `scheme/physics/quantum.scm`:

```typescript
interface QuantumVisualization {
  type: 'quantum-state';
  qubits: number;
  wavefunction: Complex[];
  visualization: 'bloch-sphere' | 'density-matrix' | 'probability-cloud';
}

// Bloch sphere visualization for single qubit
class QuantumRenderer {
  renderBlochSphere(state: QuantumState): THREE.Object3D {
    const sphere = new THREE.SphereGeometry(1, 32, 32);
    const material = new THREE.MeshPhongMaterial({
      color: 0x4444ff,
      transparent: true,
      opacity: 0.3,
      wireframe: true
    });
    const bloch = new THREE.Mesh(sphere, material);

    // State vector as arrow
    const [alpha, beta] = state.amplitudes;
    const theta = 2 * Math.acos(alpha.abs());
    const phi = beta.phase();

    const direction = new THREE.Vector3(
      Math.sin(theta) * Math.cos(phi),
      Math.sin(theta) * Math.sin(phi),
      Math.cos(theta)
    );

    const arrow = new THREE.ArrowHelper(
      direction.normalize(),
      new THREE.Vector3(0, 0, 0),
      1,
      0xff0000,
      0.2,
      0.1
    );

    const group = new THREE.Group();
    group.add(bloch);
    group.add(arrow);

    return group;
  }
}
```

### General Relativity Spacetime Curvature

Render metric tensors from `scheme/physics/gr.scm`:

```typescript
class SpacetimeRenderer {
  renderCurvature(metric: MetricTensor, resolution: number = 50): THREE.Mesh {
    const geometry = new THREE.PlaneGeometry(10, 10, resolution, resolution);
    const vertices = geometry.attributes.position.array;

    // Deform plane according to metric
    for (let i = 0; i < vertices.length; i += 3) {
      const x = vertices[i];
      const y = vertices[i + 1];

      // Schwarzschild metric curvature (example)
      const r = Math.sqrt(x * x + y * y);
      const curvature = this.computeCurvature(metric, x, y);
      vertices[i + 2] = curvature;  // Z displacement
    }

    geometry.attributes.position.needsUpdate = true;
    geometry.computeVertexNormals();

    const material = new THREE.MeshStandardMaterial({
      color: 0x00ff00,
      wireframe: false,
      side: THREE.DoubleSide
    });

    return new THREE.Mesh(geometry, material);
  }
}
```

### Consciousness Qualia Field Evolution

Animate consciousness states from `scheme/consciousness/qualia.scm`:

```typescript
interface ConsciousnessState {
  qualiaField: number[];  // E8 representation
  observationMode: 'perception' | 'action';
  coherence: number;
  timestamp: number;
}

class ConsciousnessVisualizer {
  animateQualiaEvolution(states: ConsciousnessState[]): TemporalTemplate {
    const timeline: KeyFrame[] = states.map((state, i) => ({
      time: i * 0.1,  // 100ms per state
      objects: {
        'qualia-field': {
          position: this.projectE8ToR3(state.qualiaField),
          scale: state.coherence * 2,
          color: this.coherenceToColor(state.coherence)
        },
        'observation-mode': {
          position: [state.observationMode === 'action' ? 3 : -3, 0, 0],
          scale: 1.0,
          color: state.observationMode === 'action' ? '#ff0000' : '#0000ff'
        }
      }
    }));

    return {
      duration: states.length * 0.1,
      fps: 30,
      timeline
    };
  }

  projectE8ToR3(e8Vector: number[]): [number, number, number] {
    // Stereographic projection E8 → R³
    const [x0, x1, x2, x3, x4, x5, x6, x7] = e8Vector;
    const norm = Math.sqrt(e8Vector.reduce((sum, x) => sum + x * x, 0));

    return [
      (x0 + x1) / (1 + x7 / norm),
      (x2 + x3) / (1 + x7 / norm),
      (x4 + x5 + x6) / (1 + x7 / norm)
    ];
  }

  coherenceToColor(coherence: number): string {
    const hue = coherence * 120;  // 0 (red) → 120 (green)
    return `hsl(${hue}, 100%, 50%)`;
  }
}
```

## Implementation Roadmap

### Phase 1: Template-Driven GLB Manipulation ✅ Foundational

**Goal**: Load, transform, and re-export GLB assets via CanvasL templates

**Components**:
1. GLB loader with transformation API
2. Material/texture override system
3. Template parser for model specifications
4. Export pipeline (GLB/GLTF)

**Deliverable**: `scheme/3d/glb-manipulator.scm` + `services/3d-api/glb.py`

### Phase 2: Procedural Geometry Generation ✅ Foundational

**Goal**: Generate parametric surfaces from mathematical expressions

**Components**:
1. Parametric curve/surface evaluators
2. E8 lattice projection
3. p-adic curve visualization
4. Template language for procedural specs

**Deliverable**: `scheme/3d/procedural.scm` + TypeScript generators

### Phase 3: Frame Capture Pipeline 🚧 Video Infrastructure

**Goal**: Render temporal sequences to frame buffers

**Components**:
1. Headless WebGL renderer (node-canvas or similar)
2. Frame capture API
3. Keyframe interpolation system
4. Progress monitoring

**Deliverable**: `services/video-render/capture.ts`

### Phase 4: Video Encoding Integration 🚧 Video Infrastructure

**Goal**: Encode frame sequences to video files

**Components**:
1. FFmpeg wrapper (Node.js)
2. Codec selection (H.264, VP9, AV1)
3. Batch processing system
4. Quality/compression controls

**Deliverable**: `services/video-render/encode.ts`

### Phase 5: Physics/Consciousness Visualization 🎯 MLSS Integration

**Goal**: Visualize quantum states, GR spacetime, consciousness evolution

**Components**:
1. Quantum state renderers (Bloch sphere, density matrix)
2. GR curvature visualization
3. Consciousness qualia field animation
4. Integration with existing FastAPI services

**Deliverable**: `scheme/3d/physics-viz.scm` + visualization templates

### Phase 6: End-to-End Video Generation 🎯 Complete Pipeline

**Goal**: Single command to generate video from high-level specification

**Components**:
1. Unified API: template → video file
2. CLI tool: `meta-log render consciousness-evolution.canvasl -o video.mp4`
3. Web interface: drag-and-drop template upload
4. Example gallery (quantum entanglement, spacetime warping, etc.)

**Deliverable**: `cli/render.ts` + `docs/VIDEO_GENERATION.md`

## Categorical Semantics

The complete pipeline exhibits nested adjunctions:

```
Template ⊣ Manipulation (static 3D)
   ↓ temporal extension
KeyFrames ⊣ Interpolation (temporal evolution)
   ↓ observation
FrameSequence ⊣ VideoEncoding (media synthesis)
```

Each level is a free/forgetful adjunction:
- **Templates freely construct** visual specifications (no evaluation)
- **Manipulators observe** templates as rendered geometry (evaluation)
- **KeyFrames freely construct** temporal evolution (discrete states)
- **Interpolation observes** keyframes as continuous motion (smooth evaluation)
- **Frame sequences freely construct** pixel arrays (no compression)
- **Video encoding observes** frames as compressed media (lossy evaluation)

The composition `VideoEncoding ∘ Interpolation ∘ Manipulation` is the **grand right adjoint**: it takes free template data and produces the terminal observable object (a video file).

## References

- [Babylon Integration](../../docs/research/babylon-integration.md) - Canonical 3D engine
- [3D Projections](../../docs/research/projections.md) - 2D→3D transformation theory
- [3D Computational Manifolds](../../docs/3D_COMPUTATIONAL_MANIFOLDS.md) - Visual programming framework
- [p-adic Curves](21-p-adic-curves.md) - Parametric curve theory
- [E8 Lattice](22-E8-lattice.md) - Exceptional geometry substrate
- [Computational Physics](41-Computational-Physics.md) - Quantum/GR/QFT visualization
- [Computer Vision](36-Computer-Vision.md) - Visual dual pairs framework

---

**Status**: Specification complete. Implementation phases 1-2 buildable from existing substrate. Phases 3-6 require FFmpeg integration and headless rendering infrastructure.

**Next steps**:
1. Implement GLB manipulation API
2. Add procedural geometry generators
3. Prototype frame capture with node-canvas
4. Integrate FFmpeg for video encoding

**Timeline**: Phases 1-2 (1 week), Phases 3-4 (1 week), Phases 5-6 (2 weeks) = 4 weeks to complete video generation pipeline.
