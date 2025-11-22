# 3D Computational Manifold Framework
## Complete Technical Specification v1.0

**A Unified Architecture for Computational Epistemology, Multi-Agent Intelligence, and Immersive 3D Visualization**

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Core Concepts](#core-concepts)
3. [Architecture Layers](#architecture-layers)
4. [Polynomial Type System](#polynomial-type-system)
5. [Evaluation Encoding](#evaluation-encoding)
6. [Cryptographic Spine](#cryptographic-spine)
7. [Multi-Agent Intelligence](#multi-agent-intelligence)
8. [3D Visualization System](#3d-visualization-system)
9. [Persistence & Synchronization](#persistence--synchronization)
10. [API Reference](#api-reference)
11. [Implementation Roadmap](#implementation-roadmap)

---

## 1. Executive Summary

### 1.1 Vision

The 3D Computational Manifold Framework is a **visual programming environment** where:

- **Computation is spatial**: Every expression exists as a 3D object in type-space
- **Evaluation is observable**: Reduction steps become animated particle flows
- **Intelligence is collective**: Multi-agent GNN systems learn and evolve
- **History is cryptographic**: All state changes are provably correct
- **Semantics drive geometry**: WordNet ontologies generate 3D structures

### 1.2 Key Innovation

**Unification of Four Traditionally Separate Domains:**

```
Computational Logic ─┐
                     ├─→ Polynomial Type Space ─→ 3D WebGL Manifold
Evaluation Traces  ──┤
                     ├─→ Cryptographic Chain    ─→ Provable Computation
Agent Intelligence ──┤
                     └─→ Graph Neural Networks ─→ Emergent Behavior
```

### 1.3 Use Cases

1. **Educational**: Visual debugging of functional programs
2. **Research**: Observable computational epistemology
3. **Gaming**: Emergent AI-driven 3D universes
4. **Metaverse**: Spatial programming environments
5. **Verification**: Cryptographically provable computation

---

## 2. Core Concepts

### 2.1 The Fundamental Duality

```yaml
M-Expressions:
  nature: "Descriptions of HOW to evaluate"
  syntax: "eval[plus[1; 2]; env]"
  role: "Meta-level strategy encoding"
  
S-Expressions:
  nature: "Records of WHAT was evaluated"
  syntax: "(evaluation-trace (steps ...) (result 3))"
  role: "Object-level execution trace"
  
Transformation:
  process: "M → S compilation"
  result: "Evaluation strategy becomes execution history"
```

### 2.2 The Eight-Type Polynomial Basis

**Every R5RS expression maps to an 8-dimensional type vector:**

| Index | Type      | Meaning                    |
|-------|-----------|----------------------------|
| 0     | Boolean   | Truth values               |
| 1     | Pair      | Cons cells (lists)         |
| 2     | Symbol    | Variables, identifiers     |
| 3     | Number    | Numeric values             |
| 4     | Char      | Characters                 |
| 5     | String    | Text sequences             |
| 6     | Vector    | Arrays                     |
| 7     | Procedure | Functions (+ Ports)        |

**Example:**

```scheme
(+ 1 2) → Symbol[+] + Number[1] + Number[2]
       → [0, 0, 1, 2, 0, 0, 0, 0]  ; Type vector
```

### 2.3 The Three Polynomial Views

```
Monad      = type-vector(expr)
Functor    = Monad + ast-complexity(expr)
Perceptron = Functor + network-weights(expr)
```

**Interpretation:**

- **Monad**: Base type signature
- **Functor**: Structural complexity (depth, branching)
- **Perceptron**: Network connectivity (for agent learning)

### 2.4 Type Space as 3D Coordinates

**Mapping 8D → 3D for visualization:**

```javascript
position  = [monad[0]/10, monad[1]/10, monad[2]/10]  // XYZ
rotation  = [monad[3]*0.1, monad[4]*0.1, monad[5]*0.1]  // Euler angles
scale     = 1 + monad[6]*0.5
opacity   = monad[7]/10
```

---

## 3. Architecture Layers

### 3.1 Four-Layer Stack

```
┌────────────────────────────────────────────────┐
│ Layer 1: USER INTERFACE (M-Expressions)       │
│ - NLP commands: "Create fractal cathedral"    │
│ - Voice input, gesture controls               │
│ - Strategy selection (normal/applicative/lazy)│
└────────────────────────────────────────────────┘
                      ↓
┌────────────────────────────────────────────────┐
│ Layer 2: COMPILATION (M → S Transform)        │
│ - Parse evaluation strategies                 │
│ - Generate polynomial coordinates             │
│ - Compile to executable traces                │
└────────────────────────────────────────────────┘
                      ↓
┌────────────────────────────────────────────────┐
│ Layer 3: EXECUTION ENGINE (Cryptographic)     │
│ - ManifoldSpine: State delta chain            │
│ - SHA-256 block hashing                       │
│ - Proof-of-computation verification           │
└────────────────────────────────────────────────┘
                      ↓
┌────────────────────────────────────────────────┐
│ Layer 4: VISUALIZATION (WebGL Manifold)       │
│ - 3D universe rendering                       │
│ - Particle effect evaluation traces           │
│ - Agent avatars and GNN embeddings            │
└────────────────────────────────────────────────┘
```

### 3.2 Data Flow

```
User Command → M-Expression → Polynomial Analysis → State Delta
     ↓              ↓                ↓                    ↓
 Voice NLP    Strategy Rules    Type Vectors      Cryptographic Hash
     ↓              ↓                ↓                    ↓
 "spawn 3"    applicative-order  [0,1,0,3,...]    Block #42: 0x3a7f...
     ↓              ↓                ↓                    ↓
Evaluation   Trace Generation   3D Coordinates    Broadcast to Peers
     ↓              ↓                ↓                    ↓
 Result: 6   (step-1 ...) → WebGL Universe → Replicated State
```

---

## 4. Polynomial Type System

### 4.1 Type Vector Computation

**Algorithm:**

```javascript
function typeVector(expr) {
  // Base case: primitive types
  if (typeof expr === 'boolean')  return [1,0,0,0,0,0,0,0];
  if (typeof expr === 'number')   return [0,0,0,1,0,0,0,0];
  if (typeof expr === 'string')   return [0,0,0,0,0,1,0,0];
  if (typeof expr === 'symbol')   return [0,0,1,0,0,0,0,0];
  if (typeof expr === 'function') return [0,0,0,0,0,0,0,1];
  
  // Recursive case: compound expressions
  if (Array.isArray(expr)) {
    return [0,1,0,0,0,0,0,0];  // Pair/List
  }
  
  return [0,0,0,0,0,0,0,0];  // Unknown
}
```

### 4.2 Polynomial Operations

**Addition (vector space):**

```javascript
function polyAdd(v1, v2) {
  return v1.map((val, idx) => val + v2[idx]);
}
```

**Multiplication (tensor product, simplified):**

```javascript
function polyMultiply(v1, v2) {
  return v1.map((val, idx) => (val * v2[idx]) % 10);
}
```

### 4.3 AST Complexity Analysis

**Depth-first traversal counting types:**

```javascript
function astComplexity(expr) {
  const base = typeVector(expr);
  
  if (Array.isArray(expr)) {
    return expr.reduce(
      (acc, subexpr) => polyAdd(acc, astComplexity(subexpr)),
      base
    );
  }
  
  return base;
}
```

**Example:**

```scheme
(+ (* 2 3) 4)

Step 1: typeVector(+)        = [0,0,1,0,0,0,0,0]  ; Symbol
Step 2: astComplexity(* 2 3) = [0,1,1,2,0,0,0,0]  ; Pair+Symbol+2*Number
Step 3: typeVector(4)        = [0,0,0,1,0,0,0,0]  ; Number
Result: [0,2,2,3,0,0,0,0]  ; Functor vector
```

### 4.4 Network Weights (Perceptron View)

**Simulate neural network connectivity:**

```javascript
function networkWeights(expr) {
  const functor = astComplexity(expr);
  // Triple the functor for network density
  return polyAdd(functor, polyAdd(functor, functor));
}
```

**Interpretation:** The perceptron vector represents how "connected" an expression is - used by agents for learning.

---

## 5. Evaluation Encoding

### 5.1 Evaluation Strategies as M-Expressions

**Normal-Order (lazy, outermost-first):**

```lisp
(m-strategy "normal-order"
  ((m-rule "β-reduction"
    (m-app (m-lambda (m-var X) (m-body M)) (m-arg N))
    (m-subst M X N))
   (m-rule "η-reduction"
    (m-lambda (m-var X) (m-app (m-var F) (m-var X)))
    (m-var F)
    (condition (not-free X F)))))
```

**Applicative-Order (eager, innermost-first):**

```lisp
(m-strategy "applicative-order"
  ((m-rule "β-reduction"
    (m-app (m-lambda (m-var X) (m-body M)) (m-value V))
    (m-subst M X V))
   (m-rule "primitive-application"
    (m-app (m-primitive OP) (m-values ...ARGS))
    (m-apply-primitive OP ARGS))))
```

**Lazy Evaluation (call-by-need):**

```lisp
(m-strategy "lazy"
  ((m-rule "thunk-creation"
    (m-app (m-lambda (m-var X) (m-body M)) (m-arg N))
    (m-subst M X (m-thunk N ENV)))
   (m-rule "force-thunk"
    (m-thunk (m-value V) ENV)
    V)))
```

### 5.2 Evaluation Traces as S-Expressions

**Structure:**

```scheme
(evaluation-trace
  (input <expression>)
  (strategy <strategy-name>)
  (steps
    (step-1
      (expression <current-expr>)
      (environment <env-bindings>)
      (context <evaluation-context>)
      (rule <reduction-rule-name>)
      (result <reduced-expr>))
    (step-2 ...)
    ...)
  (final-value <result>)
  (statistics
    (steps-count <n>)
    (reductions-by-type <histogram>)
    (max-stack-depth <depth>)))
```

**Example Trace:**

```scheme
;; Input: ((λx.(+ x 1)) 2)
(evaluation-trace
  (input (app (lambda (x) (+ x 1)) 2))
  (strategy applicative-order)
  (steps
    (step-1
      (expression 2)
      (rule evaluate-value)
      (result 2))
    (step-2
      (expression (lambda (x) (+ x 1)))
      (rule create-closure)
      (result (closure (x) (+ x 1) ())))
    (step-3
      (expression (app (closure (x) (+ x 1) ()) 2))
      (rule β-reduction)
      (result (+ 2 1)))
    (step-4
      (expression (+ 2 1))
      (rule primitive-application)
      (result 3)))
  (final-value 3))
```

### 5.3 Compilation: M → S Transform

**Algorithm:**

```javascript
function compileEvaluation(mExpr, program, env) {
  const strategy = extractStrategy(mExpr);
  const rules = compileRules(strategy.rules);
  const trace = [];
  
  function evalStep(expr, env, context) {
    const matchedRule = rules.find(r => r.pattern.matches(expr));
    if (!matchedRule) return expr;
    
    const reduction = matchedRule.apply(expr, env);
    trace.push({
      expression: expr,
      environment: env,
      context: context,
      rule: matchedRule.name,
      result: reduction
    });
    
    return evalStep(reduction, env, context);
  }
  
  const result = evalStep(program, env, 'top-level');
  
  return {
    input: program,
    strategy: strategy.name,
    steps: trace,
    finalValue: result
  };
}
```

---

## 6. Cryptographic Spine

### 6.1 State Delta Chain

**Structure:**

```typescript
interface StateDelta {
  action: 'spawn_universe' | 'extend_model' | 'r5rs_clause' 
        | 'metadata_update' | 'fork_branch' | 'merge_branch';
  payload: any;
  author: string;
  timestamp: number;
  previousHash: string;
  nonce: number;
}

interface StateBlock {
  delta: StateDelta;
  hash: string;  // SHA-256(delta + previousHash + nonce)
  proofOfWork?: number;
}
```

**Chain Invariant:**

```
Block[n].hash === SHA256(Block[n].delta + Block[n-1].hash + nonce)
Block[0].previousHash === "0x000...000"  // Genesis
```

### 6.2 World State

```typescript
interface WorldState {
  universes: Map<string, Universe>;
  models: Map<string, GLTFDocument>;
  semantics: Map<string, R5RSClause>;
  agents: Map<string, AgentSpec>;
  evaluations: Map<string, EvaluationTrace>;
  head: string;  // Hash of latest block
  branches: Map<string, string>;  // branchName → blockHash
}
```

### 6.3 State Transition Function

```typescript
function applyDelta(state: WorldState, delta: StateDelta): WorldState {
  const newState = cloneDeep(state);
  
  switch (delta.action) {
    case 'spawn_universe':
      const universe = createUniverse(delta.payload);
      newState.universes.set(universe.id, universe);
      break;
      
    case 'r5rs_clause':
      const clause = parseClause(delta.payload.clause);
      const interpretation = interpretClause(clause);
      newState.semantics.set(clause.id, {
        clause,
        interpretation,
        polynomial: computePolynomial(clause)
      });
      break;
      
    case 'extend_model':
      const base = newState.models.get(delta.payload.base);
      const extended = extendGLTF(base, delta.payload.extension);
      newState.models.set(delta.payload.result, extended);
      break;
      
    // ... other actions
  }
  
  return newState;
}
```

### 6.4 Proof Generation

```typescript
function generateProof(block: StateBlock, state: WorldState): Proof {
  return {
    blockHash: block.hash,
    stateRoot: merkleRoot(state),
    witnesses: {
      previousState: merkleProof(state.previous, block.delta.affects),
      newState: merkleProof(state.current, block.delta.affects),
      transition: computeTransitionProof(block.delta)
    },
    signature: sign(block.hash, privateKey)
  };
}
```

---

## 7. Multi-Agent Intelligence

### 7.1 Agent Architecture

```typescript
interface Agent {
  id: string;
  seed: number;  // Deterministic RNG
  location: string;  // Universe ID
  behavior: 'wander' | 'builder' | 'predator' | 'miner';
  
  // Q-Learning components
  qtable: Map<StateKey, Map<Action, QValue>>;
  alpha: number;  // Learning rate
  gamma: number;  // Discount factor
  epsilon: number;  // Exploration rate
  
  // Neural network
  net: TinyMLP;  // 4→8→5 neurons
  
  // Resources
  energy: number;
  memory: Map<string, any>;
}
```

### 7.2 TinyMLP (Embedded Neural Network)

```typescript
class TinyMLP {
  layers: number[];  // e.g., [4, 8, 5]
  weights: number[][];  // Serializable
  biases: number[][];
  
  forward(input: number[]): number[] {
    let activation = input;
    for (let layer of this.layers) {
      activation = layer.forward(activation);
      activation = activation.map(tanh);  // Non-linearity
    }
    return activation;
  }
  
  mutate(rate: number, scale: number): void {
    // Evolutionary update (no backprop)
    this.weights.forEach(W => {
      W.forEach((w, i) => {
        if (Math.random() < rate) {
          W[i] += (Math.random() * 2 - 1) * scale;
        }
      });
    });
  }
}
```

### 7.3 Q-Learning Policy

```typescript
function agentPolicy(agent: Agent, world: WorldState): AgentAction {
  const stateKey = computeStateKey(agent, world);
  
  // Epsilon-greedy exploration
  if (Math.random() < agent.epsilon) {
    return randomAction();
  }
  
  // Exploit: choose best Q-value
  const qValues = agent.qtable.get(stateKey);
  const bestAction = maxBy(qValues, ([action, q]) => q);
  
  // Use MLP for parameter selection
  const features = extractFeatures(agent, world);
  const mlpOutput = agent.net.forward(features);
  const params = interpretMLPOutput(mlpOutput);
  
  return {
    action: bestAction,
    params: params
  };
}
```

### 7.4 Learning Update

```typescript
function agentLearn(
  agent: Agent,
  prevWorld: WorldState,
  action: Action,
  reward: number,
  newWorld: WorldState
): void {
  const s = computeStateKey(agent, prevWorld);
  const s2 = computeStateKey(agent, newWorld);
  
  // Q-learning update
  const q = agent.qtable.get(s).get(action);
  const maxQ2 = Math.max(...agent.qtable.get(s2).values());
  const target = reward + agent.gamma * maxQ2;
  
  agent.qtable.get(s).set(action, 
    q + agent.alpha * (target - q)
  );
  
  // Evolutionary MLP update on high reward
  if (reward > 5) {
    agent.net.mutate(0.05, 0.02);
  }
}
```

### 7.5 Graph Neural Network (GCN)

**Multi-agent collective intelligence via message passing:**

```typescript
class GCN {
  layers: GCNLayer[];
  
  forward(
    nodes: Map<string, number[]>,  // nodeId → features
    edges: Edge[]                   // {from, to, weight}
  ): Map<string, number[]> {       // nodeId → embeddings
    
    let embeddings = nodes;
    
    for (const layer of this.layers) {
      const newEmbeddings = new Map();
      
      for (const [nodeId, features] of embeddings) {
        // Aggregate neighbor messages
        const neighbors = edges
          .filter(e => e.to === nodeId)
          .map(e => embeddings.get(e.from));
        
        const aggregated = neighbors.reduce(
          (sum, n) => vecAdd(sum, n),
          zeros(features.length)
        );
        
        // Transform: self + neighbors
        const selfPart = matmul(features, layer.W_self);
        const msgPart = matmul(aggregated, layer.W_msg);
        const combined = vecAdd(selfPart, msgPart);
        
        newEmbeddings.set(nodeId, combined.map(tanh));
      }
      
      embeddings = newEmbeddings;
    }
    
    return embeddings;
  }
}
```

**Graph Construction:**

```typescript
function buildAgentGraph(world: WorldState): Graph {
  const nodes = new Map();
  const edges = [];
  
  // Nodes: universes with features
  for (const [id, universe] of world.universes) {
    nodes.set(id, [
      universe.energy / 100,
      universe.entropy / 100,
      universe.stability,
      universe.ramification / 5,
      countAgents(universe) / 10,
      geometryComplexity(universe) / 1000
    ]);
  }
  
  // Edges: parent-child, spatial, semantic
  for (const [id, universe] of world.universes) {
    if (universe.parent) {
      edges.push({from: universe.parent, to: id, weight: 1.0});
    }
    
    // Spatial neighbors (within radius)
    for (const [id2, u2] of world.universes) {
      if (id !== id2 && distance(universe, u2) < 3.0) {
        edges.push({from: id, to: id2, weight: 0.5});
      }
    }
  }
  
  return {nodes, edges};
}
```

---

## 8. 3D Visualization System

### 8.1 WebGL Scene Structure

```typescript
interface WebGLScene {
  entities: Entity[];
  connections: Connection[];
  cameras: Camera[];
  lights: Light[];
  shaders: Map<string, ShaderProgram>;
  animations: Animation[];
}

interface Entity {
  id: string;
  geometry: 'sphere' | 'cube' | 'icosahedron' | 'custom';
  material: {
    type: 'standard' | 'shader';
    vertexShader?: string;
    fragmentShader?: string;
  };
  transform: {
    position: [number, number, number];
    rotation: [number, number, number];
    scale: number;
  };
  metadata: {
    polynomial: PolynomialView;
    evaluation: EvaluationTrace;
    universe: Universe;
  };
}
```

### 8.2 Polynomial GLSL Shader

```glsl
// polynomial-fragment-shader.glsl
uniform vec3 monadCoords;
uniform vec3 functorCoords;
uniform vec3 perceptronCoords;
uniform float time;

varying vec3 vPosition;
varying vec3 vNormal;

void main() {
  // Concentric rings at polynomial coordinates
  float d1 = length(vPosition - monadCoords);
  float d2 = length(vPosition - functorCoords);
  float d3 = length(vPosition - perceptronCoords);
  
  float ring1 = sin(d1 * 10.0 - time) * 0.5 + 0.5;
  float ring2 = sin(d2 * 15.0 - time * 1.5) * 0.5 + 0.5;
  float ring3 = sin(d3 * 20.0 - time * 2.0) * 0.5 + 0.5;
  
  vec3 color = vec3(
    ring1 * 0.8 + 0.2,
    ring2 * 0.6 + 0.3,
    ring3 * 0.7 + 0.2
  );
  
  // Fresnel glow
  float fresnel = pow(1.0 - dot(vNormal, vec3(0,0,1)), 3.0);
  color += vec3(0.2, 0.5, 0.8) * fresnel;
  
  gl_FragColor = vec4(color, 1.0);
}
```

### 8.3 Evaluation Trace Animation

```typescript
function animateEvaluation(trace: EvaluationTrace): Animation {
  const keyframes = [];
  let time = 0;
  
  for (const step of trace.steps) {
    const duration = stepDuration(step);
    
    keyframes.push({
      time,
      transform: stepToTransform(step),
      particleEffect: {
        type: step.rule === 'β-reduction' ? 'burst' : 'flow',
        color: ruleColor(step.rule),
        intensity: 0.8,
        lifetime: duration
      }
    });
    
    time += duration;
  }
  
  return {
    duration: time,
    keyframes,
    loop: false
  };
}
```

### 8.4 Camera Controls

```typescript
interface CameraControl {
  mode: 'orbit' | 'fly' | 'follow-evaluation';
  target: Vector3;
  distance: number;
  rotation: {azimuth: number, elevation: number};
  
  update(delta: number): void {
    switch (this.mode) {
      case 'orbit':
        this.rotation.azimuth += delta * 0.1;
        break;
      case 'follow-evaluation':
        const currentStep = getCurrentEvaluationStep();
        this.target = entityPosition(currentStep.entity);
        break;
    }
  }
}
```

---

## 9. Persistence & Synchronization

### 9.1 Local Storage (window.storage)

```typescript
class PersistenceLayer {
  async saveState(state: WorldState): Promise<void> {
    // Serialize state
    const serialized = {
      universes: Array.from(state.universes.entries()),
      models: await serializeModels(state.models),
      semantics: Array.from(state.semantics.entries()),
      agents: Array.from(state.agents.entries()),
      head: state.head
    };
    
    // Store with key-value API
    await window.storage.set(
      'manifold:state',
      JSON.stringify(serialized)
    );
    
    // Store spine blocks incrementally
    const newBlocks = getBlocksSince(state.lastSavedBlock);
    for (const block of newBlocks) {
      await window.storage.set(
        `manifold:block:${block.hash}`,
        JSON.stringify(block)
      );
    }
  }
  
  async loadState(): Promise<WorldState> {
    const data = await window.storage.get('manifold:state');
    if (!data) return initialState();
    
    const parsed = JSON.parse(data.value);
    return {
      universes: new Map(parsed.universes),
      models: await deserializeModels(parsed.models),
      semantics: new Map(parsed.semantics),
      agents: new Map(parsed.agents),
      head: parsed.head
    };
  }
}
```

### 9.2 P2P Synchronization (WebRTC)

```typescript
class P2PSync {
  peers: Map<string, RTCPeerConnection>;
  myPeerId: string;
  
  async connectToPeer(peerId: string): Promise<void> {
    const peer = new RTCPeerConnection({
      iceServers: [{urls: 'stun:stun.l.google.com:19302'}]
    });
    
    // Data channel for state deltas
    const channel = peer.createDataChannel('manifold-sync');
    
    channel.onopen = () => {
      // Send recent blocks
      const recentBlocks = getRecentBlocks(20);
      channel.send(JSON.stringify({
        type: 'sync-request',
        blocks: recentBlocks
      }));
    };
    
    channel.onmessage = (event) => {
      const message = JSON.parse(event.data);
      this.handleMessage(message, peerId);
    };
    
    this.peers.set(peerId, peer);
    
    // Signaling via MQTT
    await this.signal(peerId, await peer.createOffer());
  }
  
  handleMessage(message: any, fromPeer: string): void {
    switch (message.type) {
      case 'state-delta':
        const block = verifyAndApplyBlock(message.delta);
        if (block) {
          this.broadcastToOthers(message, fromPeer);
        }
        break;
        
      case 'sync-request':
        const missing = findMissingBlocks(message.blocks);
        this.sendBlocks(missing, fromPeer);
        break;
    }
  }
}
```

### 9.3 Conflict Resolution

```typescript
function resolveConflict(
  localChain: StateBlock[],
  remoteChain: StateBlock[]
): StateBlock[] {
  // Find common ancestor
  const commonAncestor = findCommonAncestor(localChain, remoteChain);
  
  // Choose longer chain (simple PoW)
  if (remoteChain.length > localChain.length) {
    return remoteChain;
  }
  
  // If equal length, choose chain with more work
  const localWork = sumWork(localChain);
  const remoteWork = sumWork(remoteChain);
  
  return remoteWork > localWork ? remoteChain : localChain;
}
```

---

## 10. API Reference

### 10.1 Core API

```typescript
class ManifoldOS {
  // Initialization
  constructor(config: ManifoldConfig);
  async initialize(): Promise<void>;
  
  // Universe management
  spawnUniverse(expr: Expression): Universe;
  getUniverse(id: string): Universe | null;
  destroyUniverse(id: string): void;
  
  // Evaluation
  evaluate(
    expr: Expression,
    strategy: EvaluationStrategy
  ): EvaluationTrace;
  
  // Agent management
  createAgent(spec: AgentSpec): Agent;
  stepAgents(): void;
  trainAgent(agentId: string, episodes: number): void;
  
  // State management
  getState(): Worl