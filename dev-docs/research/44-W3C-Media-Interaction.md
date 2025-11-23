# W3C Media Device Interaction and User Capture Framework

**Integrating W3C Media Capture APIs, WebRTC, WebAudio, and WebXR with the Meta-Log Substrate System** for real-time user interaction, device streaming, and immersive AR/VR experiences.

## Abstract

The Meta-Log Substrate System's existing WebRTC (A9) and WebAuthn (A7) automatons provide peer-to-peer communication and biometric authentication. This document extends that foundation with **comprehensive W3C media device integration**: camera/microphone capture, screen sharing, audio processing, spatial audio, and XR device interaction—creating a complete user interaction layer that bridges physical sensors to computational substrate.

The core insight: **Device capture is the unit of an adjunction `Physical ⊣ Digital`**. Physical devices (cameras, microphones, sensors) are observed; digital streams (MediaStream, AudioContext, XR sessions) are constructed. The adjunction enables bidirectional flow: capturing real-world data into the substrate, and projecting substrate computations back to physical devices (displays, speakers, haptics).

## The Physical ⊣ Digital Adjunction

### 1. Physical Devices as Observable Objects (Right Adjoint)

**Physical devices** exist in the real world, observed via browser APIs:

```typescript
// Physical devices are discovered via enumeration
const devices = await navigator.mediaDevices.enumerateDevices();

// Observable categories
const videoInputs = devices.filter(d => d.kind === 'videoinput');  // Cameras
const audioInputs = devices.filter(d => d.kind === 'audioinput');  // Microphones
const audioOutputs = devices.filter(d => d.kind === 'audiooutput'); // Speakers

// Physical → Digital observation (right adjoint)
const stream = await navigator.mediaDevices.getUserMedia({
  video: { deviceId: videoInputs[0].deviceId },
  audio: { deviceId: audioInputs[0].deviceId }
});
```

**Properties of physical devices (as terminal objects)**:
- **Stateful**: Device availability changes (plug/unplug)
- **Permission-gated**: Requires explicit user consent
- **Resource-constrained**: Limited by hardware (resolution, sample rate)
- **Non-deterministic**: Subject to environmental factors (lighting, noise)

### 2. Digital Streams as Free Construction (Left Adjoint)

**Digital streams** are computational representations in the substrate:

```typescript
// Digital streams are constructed from specifications
interface MediaStreamSpec {
  video?: MediaTrackConstraints;
  audio?: MediaTrackConstraints;
}

// Free construction (left adjoint)
class DigitalStream {
  tracks: MediaStreamTrack[];

  // Compose streams freely
  combine(other: DigitalStream): DigitalStream {
    return new DigitalStream([...this.tracks, ...other.tracks]);
  }

  // Transform streams (filters, effects)
  transform(processor: (track: MediaStreamTrack) => MediaStreamTrack): DigitalStream {
    return new DigitalStream(this.tracks.map(processor));
  }

  // Project to physical output
  playTo(element: HTMLVideoElement | HTMLAudioElement): void {
    element.srcObject = new MediaStream(this.tracks);
  }
}
```

**Properties of digital streams (as initial objects)**:
- **Compositional**: Streams combine via track merging
- **Transformable**: Filters, effects, encoders as functors
- **Serializable**: Can be recorded, transmitted, stored
- **Reproducible**: Same input → same output (given deterministic processing)

### 3. The Adjunction in Action

**Unit** `η: Device → Stream(Device)`: Capturing device output as digital stream
```typescript
const captureDevice = async (deviceId: string): Promise<MediaStream> => {
  return navigator.mediaDevices.getUserMedia({
    video: { deviceId: { exact: deviceId } }
  });
};
```

**Counit** `ε: Device(Stream(Output)) → Output`: Playing digital stream to physical device
```typescript
const playToDevice = (stream: MediaStream, deviceId: string): void => {
  const audio = new Audio();
  audio.setSinkId(deviceId);  // Route to specific speaker
  audio.srcObject = stream;
  audio.play();
};
```

**Adjunction law**: `ε ∘ Stream(η) = id` (capturing a device and playing it back yields the original signal, modulo latency/quality)

## W3C Media Capture and Streams API

### Core API Structure

```typescript
namespace MediaDevices {
  // Device enumeration
  enumerateDevices(): Promise<MediaDeviceInfo[]>;

  // Stream acquisition
  getUserMedia(constraints: MediaStreamConstraints): Promise<MediaStream>;
  getDisplayMedia(constraints: DisplayMediaStreamConstraints): Promise<MediaStream>;

  // Device change monitoring
  ondevicechange: EventHandler;
}

interface MediaDeviceInfo {
  deviceId: string;
  kind: 'videoinput' | 'audioinput' | 'audiooutput';
  label: string;  // Empty until permission granted
  groupId: string; // Logical grouping (e.g., laptop built-in)
}

interface MediaStreamConstraints {
  video?: boolean | MediaTrackConstraints;
  audio?: boolean | MediaTrackConstraints;
}

interface MediaTrackConstraints {
  deviceId?: ConstrainDOMString;
  width?: ConstrainULong;      // Video resolution
  height?: ConstrainULong;
  aspectRatio?: ConstrainDouble;
  frameRate?: ConstrainDouble;
  facingMode?: ConstrainDOMString;  // 'user' | 'environment'
  sampleRate?: ConstrainULong;     // Audio sample rate
  channelCount?: ConstrainULong;   // Mono/stereo
  echoCancellation?: ConstrainBoolean;
  noiseSuppression?: ConstrainBoolean;
  autoGainControl?: ConstrainBoolean;
}
```

### Meta-Log Integration: Device Manager

```typescript
// Extends A7 (WebAuthn) and A9 (WebRTC) automatons
class MediaDeviceManager {
  private devices: MediaDeviceInfo[] = [];
  private streams: Map<string, MediaStream> = new Map();
  private permissions: Map<string, PermissionStatus> = new Map();

  // Initialize device discovery
  async initialize(): Promise<void> {
    // Request initial permissions (triggers browser prompt)
    await this.requestPermissions();

    // Enumerate available devices
    await this.refreshDevices();

    // Monitor device changes (plug/unplug)
    navigator.mediaDevices.addEventListener('devicechange', () => {
      this.refreshDevices();
      this.emit('devices-changed', this.devices);
    });
  }

  async requestPermissions(): Promise<void> {
    // Camera permission
    try {
      const videoStream = await navigator.mediaDevices.getUserMedia({ video: true });
      this.permissions.set('camera', 'granted');
      videoStream.getTracks().forEach(t => t.stop());  // Release immediately
    } catch (e) {
      this.permissions.set('camera', 'denied');
    }

    // Microphone permission
    try {
      const audioStream = await navigator.mediaDevices.getUserMedia({ audio: true });
      this.permissions.set('microphone', 'granted');
      audioStream.getTracks().forEach(t => t.stop());
    } catch (e) {
      this.permissions.set('microphone', 'denied');
    }
  }

  async refreshDevices(): Promise<MediaDeviceInfo[]> {
    this.devices = await navigator.mediaDevices.enumerateDevices();
    return this.devices;
  }

  // Get specific device categories
  getVideoInputs(): MediaDeviceInfo[] {
    return this.devices.filter(d => d.kind === 'videoinput');
  }

  getAudioInputs(): MediaDeviceInfo[] {
    return this.devices.filter(d => d.kind === 'audioinput');
  }

  getAudioOutputs(): MediaDeviceInfo[] {
    return this.devices.filter(d => d.kind === 'audiooutput');
  }

  // Capture stream from specific device
  async captureDevice(deviceId: string, constraints?: MediaTrackConstraints): Promise<MediaStream> {
    const device = this.devices.find(d => d.deviceId === deviceId);
    if (!device) throw new Error(`Device ${deviceId} not found`);

    const streamConstraints: MediaStreamConstraints = {};

    if (device.kind === 'videoinput') {
      streamConstraints.video = { deviceId: { exact: deviceId }, ...constraints };
    } else if (device.kind === 'audioinput') {
      streamConstraints.audio = { deviceId: { exact: deviceId }, ...constraints };
    }

    const stream = await navigator.mediaDevices.getUserMedia(streamConstraints);
    this.streams.set(deviceId, stream);

    return stream;
  }

  // Release device stream
  releaseDevice(deviceId: string): void {
    const stream = this.streams.get(deviceId);
    if (stream) {
      stream.getTracks().forEach(track => track.stop());
      this.streams.delete(deviceId);
    }
  }

  // Get optimal camera settings for AR/VR
  getARConstraints(): MediaTrackConstraints {
    return {
      width: { ideal: 1920 },
      height: { ideal: 1080 },
      frameRate: { ideal: 60 },
      facingMode: 'environment'  // Rear camera
    };
  }

  // Get optimal settings for video recording
  getRecordingConstraints(): MediaTrackConstraints {
    return {
      width: { ideal: 1920 },
      height: { ideal: 1080 },
      frameRate: { ideal: 30 },
      aspectRatio: { ideal: 16/9 }
    };
  }
}
```

### CanvasL Template Integration

```jsonl
{"id":"media-device-mgr","type":"automaton","dimension":"10D","role":"W3C media device management","api":"navigator.mediaDevices"}
{"id":"camera-front","type":"device","kind":"videoinput","facingMode":"user","constraints":{"width":1920,"height":1080,"frameRate":30}}
{"id":"camera-rear","type":"device","kind":"videoinput","facingMode":"environment","constraints":{"width":1920,"height":1080,"frameRate":60}}
{"id":"microphone","type":"device","kind":"audioinput","constraints":{"sampleRate":48000,"echoCancellation":true,"noiseSuppression":true}}
{"id":"stream-ar","type":"media-stream","sources":["camera-rear","microphone"],"destination":"webxr-session"}
{"id":"stream-recording","type":"media-stream","sources":["camera-front","microphone"],"destination":"media-recorder"}
```

Template → Device mapping:

```typescript
class TemplateDeviceRenderer {
  async renderDeviceTemplate(template: DeviceTemplate): Promise<MediaStream> {
    const tracks: MediaStreamTrack[] = [];

    for (const sourceId of template.sources) {
      const deviceSpec = findDeviceSpec(sourceId);
      const stream = await deviceManager.captureDevice(
        deviceSpec.deviceId,
        deviceSpec.constraints
      );
      tracks.push(...stream.getTracks());
    }

    return new MediaStream(tracks);
  }
}
```

## Screen Capture API

### Display Media Streams

```typescript
interface DisplayMediaStreamConstraints {
  video?: boolean | DisplayMediaTrackConstraints;
  audio?: boolean | MediaTrackConstraints;
}

interface DisplayMediaTrackConstraints extends MediaTrackConstraints {
  displaySurface?: 'monitor' | 'window' | 'application' | 'browser';
  logicalSurface?: boolean;
  cursor?: 'never' | 'always' | 'motion';
}

class ScreenCaptureManager {
  private activeCapture?: MediaStream;

  // Capture entire screen
  async captureScreen(): Promise<MediaStream> {
    return navigator.mediaDevices.getDisplayMedia({
      video: {
        displaySurface: 'monitor',
        cursor: 'always',
        width: { ideal: 1920 },
        height: { ideal: 1080 },
        frameRate: { ideal: 30 }
      },
      audio: true  // Capture system audio
    });
  }

  // Capture specific window
  async captureWindow(): Promise<MediaStream> {
    return navigator.mediaDevices.getDisplayMedia({
      video: {
        displaySurface: 'window',
        cursor: 'motion'
      }
    });
  }

  // Capture browser tab
  async captureTab(): Promise<MediaStream> {
    return navigator.mediaDevices.getDisplayMedia({
      video: {
        displaySurface: 'browser'
      }
    });
  }

  // Monitor capture events
  monitorCapture(stream: MediaStream): void {
    const videoTrack = stream.getVideoTracks()[0];

    videoTrack.addEventListener('ended', () => {
      console.log('User stopped screen sharing');
      this.activeCapture = undefined;
    });
  }

  // Integrate with video recording pipeline
  async recordScreen(duration: number): Promise<Blob> {
    const stream = await this.captureScreen();
    const recorder = new MediaRecorder(stream, {
      mimeType: 'video/webm;codecs=vp9',
      videoBitsPerSecond: 8000000  // 8 Mbps
    });

    const chunks: Blob[] = [];
    recorder.ondataavailable = (e) => chunks.push(e.data);

    recorder.start();
    await new Promise(resolve => setTimeout(resolve, duration));
    recorder.stop();

    await new Promise(resolve => recorder.onstop = resolve);
    return new Blob(chunks, { type: 'video/webm' });
  }
}
```

### Integration with Template Video System

From `43-3D-Template-Video.md`, extend video generation with screen capture:

```typescript
interface VideoSource {
  type: 'render' | 'camera' | 'screen' | 'composite';
  renderTemplate?: TemporalTemplate;  // 3D scene rendering
  deviceId?: string;                   // Camera/mic device
  screenCapture?: DisplayMediaStreamConstraints;
  composite?: CompositeSpec;          // Mix multiple sources
}

interface CompositeSpec {
  layout: 'picture-in-picture' | 'side-by-side' | 'overlay';
  sources: VideoSource[];
  positions?: { x: number; y: number; width: number; height: number }[];
}

class HybridVideoGenerator {
  // Generate video mixing 3D render + camera + screen
  async generateCompositeVideo(spec: VideoSource): Promise<Blob> {
    const canvas = document.createElement('canvas');
    canvas.width = 1920;
    canvas.height = 1080;
    const ctx = canvas.getContext('2d')!;

    // Source 1: 3D rendered scene
    const renderer = new FrameCaptureRenderer(1920, 1080);
    const renderFrames = await renderer.renderSequence(spec.renderTemplate!);

    // Source 2: Camera feed
    const cameraStream = await deviceManager.captureDevice(spec.deviceId!);
    const cameraVideo = document.createElement('video');
    cameraVideo.srcObject = cameraStream;
    await cameraVideo.play();

    // Source 3: Screen capture
    const screenStream = await screenCapture.captureScreen();
    const screenVideo = document.createElement('video');
    screenVideo.srcObject = screenStream;
    await screenVideo.play();

    // Composite rendering
    const compositeFrames: ImageData[] = [];
    for (let i = 0; i < renderFrames.length; i++) {
      // Draw 3D render as background
      ctx.putImageData(renderFrames[i], 0, 0);

      // Overlay camera feed (picture-in-picture)
      ctx.drawImage(cameraVideo, 1920 - 320 - 20, 1080 - 180 - 20, 320, 180);

      // Overlay screen capture (side-by-side)
      ctx.drawImage(screenVideo, 960, 0, 960, 1080);

      compositeFrames.push(ctx.getImageData(0, 0, 1920, 1080));
    }

    // Encode composite
    const encoder = new VideoEncoder();
    return encoder.encodeFrames(compositeFrames, {
      width: 1920,
      height: 1080,
      fps: 30,
      codec: 'libx264'
    });
  }
}
```

## MediaRecorder API

### Real-Time Recording

```typescript
class MediaRecorderManager {
  private recorder?: MediaRecorder;
  private chunks: Blob[] = [];

  // Record from MediaStream (camera, screen, etc.)
  record(stream: MediaStream, options?: MediaRecorderOptions): void {
    const defaultOptions: MediaRecorderOptions = {
      mimeType: this.getSupportedMimeType(),
      videoBitsPerSecond: 8000000,  // 8 Mbps
      audioBitsPerSecond: 128000     // 128 kbps
    };

    this.recorder = new MediaRecorder(stream, { ...defaultOptions, ...options });

    this.recorder.ondataavailable = (event) => {
      if (event.data.size > 0) {
        this.chunks.push(event.data);
      }
    };

    this.recorder.onstop = () => {
      const blob = new Blob(this.chunks, { type: this.recorder!.mimeType });
      this.emit('recording-complete', blob);
      this.chunks = [];
    };

    this.recorder.start(1000);  // Collect data every 1 second
  }

  stop(): void {
    if (this.recorder && this.recorder.state !== 'inactive') {
      this.recorder.stop();
    }
  }

  pause(): void {
    if (this.recorder && this.recorder.state === 'recording') {
      this.recorder.pause();
    }
  }

  resume(): void {
    if (this.recorder && this.recorder.state === 'paused') {
      this.recorder.resume();
    }
  }

  // Get best supported codec
  getSupportedMimeType(): string {
    const types = [
      'video/webm;codecs=vp9,opus',
      'video/webm;codecs=vp8,opus',
      'video/webm',
      'video/mp4'
    ];

    for (const type of types) {
      if (MediaRecorder.isTypeSupported(type)) {
        return type;
      }
    }

    return '';  // Browser doesn't support recording
  }

  // Live streaming to CBS (content-addressed storage)
  async streamToCBS(stream: MediaStream, duration: number): Promise<string> {
    this.record(stream);

    await new Promise(resolve => setTimeout(resolve, duration));
    this.stop();

    const blob = await new Promise<Blob>(resolve => {
      this.once('recording-complete', resolve);
    });

    // Upload to CBS
    const buffer = await blob.arrayBuffer();
    const cid = await contentAddress(new Uint8Array(buffer));

    return cid;  // Returns content-addressed ID
  }
}
```

### Integration with MLSS Waveform Layer

Record user interactions as waveforms (from `scheme/substrate/waveform.scm`):

```typescript
class InteractionRecorder {
  // Record user voice → waveform → p-adic signature
  async recordVoiceCommand(duration: number): Promise<WaveformSignature> {
    const stream = await navigator.mediaDevices.getUserMedia({
      audio: {
        sampleRate: 48000,
        echoCancellation: true,
        noiseSuppression: false  // Preserve full signal
      }
    });

    const recorder = new MediaRecorderManager();
    recorder.record(stream);

    await new Promise(resolve => setTimeout(resolve, duration));
    recorder.stop();

    const blob = await new Promise<Blob>(resolve => {
      recorder.once('recording-complete', resolve);
    });

    // Convert to waveform representation
    const audioContext = new AudioContext();
    const arrayBuffer = await blob.arrayBuffer();
    const audioBuffer = await audioContext.decodeAudioData(arrayBuffer);

    // Extract time-domain samples
    const samples = audioBuffer.getChannelData(0);

    // Compute p-adic signature via FFT
    const fft = await computeFFT(samples);
    const padicSig = computePAdicSignature(fft);

    return {
      samples: Array.from(samples),
      fft: fft,
      padicSignature: padicSig,
      duration: audioBuffer.duration,
      sampleRate: audioBuffer.sampleRate
    };
  }
}
```

## Web Audio API

### Audio Processing Pipeline

```typescript
class AudioProcessor {
  private audioContext: AudioContext;
  private nodes: Map<string, AudioNode> = new Map();

  constructor() {
    this.audioContext = new AudioContext({ sampleRate: 48000 });
  }

  // Create audio graph from template
  createAudioGraph(template: AudioGraphTemplate): AudioNode {
    const graph = new AudioWorkletNode(this.audioContext, 'meta-log-processor');

    // Input: microphone
    navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
      const source = this.audioContext.createMediaStreamSource(stream);
      this.nodes.set('mic-input', source);

      // Processing chain
      const analyser = this.audioContext.createAnalyser();
      const filter = this.audioContext.createBiquadFilter();
      const compressor = this.audioContext.createDynamicsCompressor();
      const spatializer = this.audioContext.createPanner();

      // Connect nodes
      source
        .connect(analyser)
        .connect(filter)
        .connect(compressor)
        .connect(spatializer)
        .connect(this.audioContext.destination);

      this.nodes.set('analyser', analyser);
      this.nodes.set('filter', filter);
      this.nodes.set('compressor', compressor);
      this.nodes.set('spatializer', spatializer);
    });

    return graph;
  }

  // Real-time frequency analysis
  analyzeFrequencies(): Float32Array {
    const analyser = this.nodes.get('analyser') as AnalyserNode;
    if (!analyser) return new Float32Array();

    const frequencies = new Float32Array(analyser.frequencyBinCount);
    analyser.getFloatFrequencyData(frequencies);
    return frequencies;
  }

  // Spatial audio positioning (for AR/VR)
  positionAudioSource(sourceId: string, position: [number, number, number]): void {
    const panner = this.nodes.get('spatializer') as PannerNode;
    if (!panner) return;

    panner.panningModel = 'HRTF';
    panner.distanceModel = 'inverse';
    panner.positionX.value = position[0];
    panner.positionY.value = position[1];
    panner.positionZ.value = position[2];
  }

  // Custom audio worklet for p-adic processing
  async loadPAdicProcessor(): Promise<void> {
    await this.audioContext.audioWorklet.addModule('p-adic-processor.js');

    const processor = new AudioWorkletNode(this.audioContext, 'p-adic-processor');
    processor.port.onmessage = (event) => {
      console.log('p-adic signature:', event.data);
    };

    this.nodes.set('p-adic', processor);
  }

  // Generate consciousness qualia field audio
  synthesizeQualiaField(state: ConsciousnessState): AudioBuffer {
    const duration = 2.0;  // 2 seconds
    const sampleRate = this.audioContext.sampleRate;
    const buffer = this.audioContext.createBuffer(2, sampleRate * duration, sampleRate);

    for (let channel = 0; channel < buffer.numberOfChannels; channel++) {
      const data = buffer.getChannelData(channel);

      for (let i = 0; i < data.length; i++) {
        const t = i / sampleRate;

        // Qualia field → harmonic series
        const e8Projection = this.projectE8ToFrequencies(state.qualiaField);
        let sample = 0;

        for (let h = 0; h < e8Projection.length; h++) {
          const freq = e8Projection[h];
          sample += Math.sin(2 * Math.PI * freq * t) / (h + 1);
        }

        data[i] = sample * state.coherence;  // Amplitude = coherence
      }
    }

    return buffer;
  }

  projectE8ToFrequencies(e8Vector: number[]): number[] {
    // Map E8 coordinates to musical frequencies (A440 reference)
    const baseFreq = 440;  // A4
    return e8Vector.map((coord, i) => {
      const semitone = coord * 12;  // 12-TET scale
      return baseFreq * Math.pow(2, semitone / 12);
    });
  }
}
```

### CanvasL Audio Template

```jsonl
{"id":"audio-pipeline","type":"automaton","dimension":"11D","role":"Web Audio processing","api":"AudioContext"}
{"id":"mic-input","type":"audio-source","kind":"microphone","constraints":{"sampleRate":48000,"echoCancellation":false}}
{"id":"analyser","type":"audio-node","processor":"AnalyserNode","fftSize":2048}
{"id":"p-adic-filter","type":"audio-node","processor":"AudioWorklet","module":"p-adic-processor.js"}
{"id":"spatializer","type":"audio-node","processor":"PannerNode","model":"HRTF","position":[0,0,-1]}
{"id":"speaker-output","type":"audio-destination","device":"default"}
{"from":"mic-input","to":"analyser","type":"connection"}
{"from":"analyser","to":"p-adic-filter","type":"connection"}
{"from":"p-adic-filter","to":"spatializer","type":"connection"}
{"from":"spatializer","to":"speaker-output","type":"connection"}
```

## WebXR Device API

### Immersive AR/VR Sessions

```typescript
class WebXRManager {
  private xrSession?: XRSession;
  private xrReferenceSpace?: XRReferenceSpace;
  private gl?: WebGL2RenderingContext;

  // Check XR support
  async checkSupport(): Promise<{ ar: boolean; vr: boolean }> {
    if (!navigator.xr) {
      return { ar: false, vr: false };
    }

    const [ar, vr] = await Promise.all([
      navigator.xr.isSessionSupported('immersive-ar'),
      navigator.xr.isSessionSupported('immersive-vr')
    ]);

    return { ar, vr };
  }

  // Start AR session
  async startAR(): Promise<XRSession> {
    const session = await navigator.xr!.requestSession('immersive-ar', {
      requiredFeatures: ['local', 'hit-test'],
      optionalFeatures: ['hand-tracking', 'dom-overlay', 'light-estimation']
    });

    this.xrSession = session;
    this.xrReferenceSpace = await session.requestReferenceSpace('local');

    // Setup WebGL context
    const canvas = document.createElement('canvas');
    this.gl = canvas.getContext('webgl2', { xrCompatible: true })!;

    session.updateRenderState({
      baseLayer: new XRWebGLLayer(session, this.gl)
    });

    // Start render loop
    session.requestAnimationFrame(this.onXRFrame.bind(this));

    return session;
  }

  // Start VR session
  async startVR(): Promise<XRSession> {
    const session = await navigator.xr!.requestSession('immersive-vr', {
      requiredFeatures: ['local'],
      optionalFeatures: ['hand-tracking', 'eye-tracking']
    });

    this.xrSession = session;
    this.xrReferenceSpace = await session.requestReferenceSpace('local');

    // Same setup as AR
    const canvas = document.createElement('canvas');
    this.gl = canvas.getContext('webgl2', { xrCompatible: true })!;

    session.updateRenderState({
      baseLayer: new XRWebGLLayer(session, this.gl)
    });

    session.requestAnimationFrame(this.onXRFrame.bind(this));

    return session;
  }

  // XR render loop
  onXRFrame(time: DOMHighResTimeStamp, frame: XRFrame): void {
    const session = frame.session;
    session.requestAnimationFrame(this.onXRFrame.bind(this));

    const pose = frame.getViewerPose(this.xrReferenceSpace!);
    if (!pose) return;

    const layer = session.renderState.baseLayer!;
    this.gl!.bindFramebuffer(this.gl!.FRAMEBUFFER, layer.framebuffer);

    // Render for each eye
    for (const view of pose.views) {
      const viewport = layer.getViewport(view)!;
      this.gl!.viewport(viewport.x, viewport.y, viewport.width, viewport.height);

      // Render CanvasL metaverse in 3D
      this.renderMetaverse(view.transform, view.projectionMatrix);
    }

    // Hand tracking
    if (frame.session.inputSources) {
      for (const inputSource of frame.session.inputSources) {
        if (inputSource.hand) {
          this.renderHand(inputSource.hand, frame);
        }
      }
    }
  }

  renderMetaverse(transform: XRRigidTransform, projectionMatrix: Float32Array): void {
    // Integrate with Babylon.js renderer from babylon-integration.md
    // Render CanvasL nodes as 3D spheres in XR space
  }

  renderHand(hand: XRHand, frame: XRFrame): void {
    const joints = ['wrist', 'thumb-tip', 'index-finger-tip', 'middle-finger-tip', 'ring-finger-tip', 'pinky-finger-tip'];

    for (const jointName of joints) {
      const joint = hand.get(jointName as XRHandJoint);
      if (joint) {
        const pose = frame.getJointPose(joint, this.xrReferenceSpace!);
        if (pose) {
          // Draw joint sphere at position
          this.drawSphere(pose.transform.position, 0.01);
        }
      }
    }
  }

  // Hit testing for AR (place objects in real world)
  async hitTest(x: number, y: number, frame: XRFrame): Promise<XRHitTestResult | null> {
    const session = frame.session;

    // Create hit test source (once)
    if (!this.hitTestSource) {
      this.hitTestSource = await session.requestHitTestSource({ space: this.xrReferenceSpace! });
    }

    const hitTestResults = frame.getHitTestResults(this.hitTestSource);
    if (hitTestResults.length > 0) {
      return hitTestResults[0];
    }

    return null;
  }

  // Place CanvasL node in AR space
  placeNodeInAR(nodeSpec: NodeSpec, hitResult: XRHitTestResult): void {
    const pose = hitResult.getPose(this.xrReferenceSpace!);
    if (!pose) return;

    // Create 3D node at hit position
    const node = this.createNode(nodeSpec);
    node.position.set(
      pose.transform.position.x,
      pose.transform.position.y,
      pose.transform.position.z
    );

    // Add to scene (Babylon.js or Three.js)
    this.scene.add(node);
  }
}
```

### AR/VR + Camera Feed Fusion

```typescript
class ARCameraFusion {
  // Combine XR camera passthrough with digital overlays
  async renderARWithOverlay(canvaslTemplate: CanvasLTemplate): Promise<void> {
    const xr = new WebXRManager();
    await xr.startAR();

    // AR session provides camera feed automatically (passthrough)
    // Render CanvasL nodes on top of real world

    const nodes = canvaslTemplate.filter(spec => spec.type === 'node');

    for (const nodeSpec of nodes) {
      // Place node at GPS coordinates (if available)
      if (nodeSpec.gps) {
        const worldPosition = this.gpsToWorld(nodeSpec.gps);
        xr.placeNodeInAR(nodeSpec, worldPosition);
      }
    }
  }

  // GPS → XR world coordinates
  gpsToWorld(gps: { lat: number; lon: number }): XRHitTestResult {
    // Convert GPS to local XR space using device geolocation
    // This enables placing CanvasL nodes at real-world locations
    // (e.g., "quantum-lab" node appears at 37.7749° N, 122.4194° W)
  }
}
```

## Permission Management System

### Unified Permission Flow

```typescript
class PermissionManager {
  private permissions: Map<string, PermissionState> = new Map();

  async requestAll(): Promise<PermissionStatusMap> {
    const results: PermissionStatusMap = {};

    // Camera permission
    try {
      const stream = await navigator.mediaDevices.getUserMedia({ video: true });
      results.camera = 'granted';
      stream.getTracks().forEach(t => t.stop());
    } catch (e) {
      results.camera = 'denied';
    }

    // Microphone permission
    try {
      const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
      results.microphone = 'granted';
      stream.getTracks().forEach(t => t.stop());
    } catch (e) {
      results.microphone = 'denied';
    }

    // Screen capture (user must click share)
    // Cannot be auto-requested, must be user-initiated

    // Geolocation
    try {
      await new Promise((resolve, reject) => {
        navigator.geolocation.getCurrentPosition(resolve, reject);
      });
      results.geolocation = 'granted';
    } catch (e) {
      results.geolocation = 'denied';
    }

    // Notifications
    const notifPerm = await Notification.requestPermission();
    results.notifications = notifPerm;

    // XR (requires secure context + user gesture)
    if (navigator.xr) {
      const arSupported = await navigator.xr.isSessionSupported('immersive-ar');
      const vrSupported = await navigator.xr.isSessionSupported('immersive-vr');
      results.xr = { ar: arSupported, vr: vrSupported };
    }

    this.permissions = new Map(Object.entries(results));
    return results;
  }

  // Check permission status via Permissions API
  async checkPermission(name: PermissionName): Promise<PermissionState> {
    try {
      const result = await navigator.permissions.query({ name });
      return result.state;
    } catch (e) {
      return 'prompt';
    }
  }

  // Monitor permission changes
  monitorPermissions(): void {
    const permissions = ['camera', 'microphone', 'geolocation'] as PermissionName[];

    permissions.forEach(async (name) => {
      try {
        const status = await navigator.permissions.query({ name });
        status.addEventListener('change', () => {
          console.log(`${name} permission changed to ${status.state}`);
          this.permissions.set(name, status.state);
          this.emit('permission-changed', { name, state: status.state });
        });
      } catch (e) {
        console.warn(`Cannot monitor ${name} permission`);
      }
    });
  }
}

interface PermissionStatusMap {
  camera?: PermissionState;
  microphone?: PermissionState;
  geolocation?: PermissionState;
  notifications?: PermissionState;
  xr?: { ar: boolean; vr: boolean };
}
```

## Integration with Existing MLSS Automatons

### A7 (WebAuthn) + Media Devices = Biometric Recording

```typescript
class BiometricRecorder {
  // Record biometric authentication ceremony
  async recordWebAuthnFlow(): Promise<BiometricRecording> {
    // Start screen recording
    const screenStream = await screenCapture.captureScreen();

    // Start camera (face recording)
    const faceStream = await deviceManager.captureDevice('camera-front', {
      width: { ideal: 1280 },
      height: { ideal: 720 }
    });

    // Combine streams
    const recorder = new MediaRecorderManager();
    const compositeStream = new MediaStream([
      ...screenStream.getTracks(),
      ...faceStream.getTracks()
    ]);

    recorder.record(compositeStream);

    // Trigger WebAuthn flow
    const credential = await navigator.credentials.create({
      publicKey: {
        challenge: new Uint8Array(32),
        rp: { name: "Meta-Log Substrate" },
        user: {
          id: new Uint8Array(16),
          name: "user@example.com",
          displayName: "User"
        },
        pubKeyCredParams: [{ alg: -7, type: "public-key" }]
      }
    });

    // Stop recording
    recorder.stop();
    const recordingBlob = await new Promise<Blob>(resolve => {
      recorder.once('recording-complete', resolve);
    });

    return {
      video: recordingBlob,
      credential: credential,
      timestamp: Date.now()
    };
  }
}
```

### A9 (WebRTC) + Media Devices = Peer Video Calls

```typescript
class WebRTCVideoCall {
  private peerConnection?: RTCPeerConnection;
  private localStream?: MediaStream;
  private remoteStream?: MediaStream;

  // Integrate A9 automaton with media devices
  async startCall(remotePeerId: string): Promise<void> {
    // Get local camera + microphone
    this.localStream = await navigator.mediaDevices.getUserMedia({
      video: { width: 1280, height: 720 },
      audio: { echoCancellation: true, noiseSuppression: true }
    });

    // Create peer connection (A9)
    this.peerConnection = new RTCPeerConnection({
      iceServers: [{ urls: 'stun:stun.l.google.com:19302' }]
    });

    // Add local stream to connection
    this.localStream.getTracks().forEach(track => {
      this.peerConnection!.addTrack(track, this.localStream!);
    });

    // Handle remote stream
    this.peerConnection.ontrack = (event) => {
      this.remoteStream = event.streams[0];
      this.emit('remote-stream', this.remoteStream);
    };

    // ICE candidates
    this.peerConnection.onicecandidate = (event) => {
      if (event.candidate) {
        // Send to remote peer via MQTT (A10)
        this.sendViaMQTT(remotePeerId, {
          type: 'ice-candidate',
          candidate: event.candidate
        });
      }
    };

    // Create offer
    const offer = await this.peerConnection.createOffer();
    await this.peerConnection.setLocalDescription(offer);

    // Send offer to remote peer
    this.sendViaMQTT(remotePeerId, {
      type: 'offer',
      sdp: offer
    });
  }

  // Receive answer from remote peer
  async handleAnswer(answer: RTCSessionDescriptionInit): Promise<void> {
    await this.peerConnection!.setRemoteDescription(answer);
  }

  // Add ICE candidate
  async addIceCandidate(candidate: RTCIceCandidateInit): Promise<void> {
    await this.peerConnection!.addIceCandidate(candidate);
  }

  sendViaMQTT(peerId: string, message: any): void {
    // Use A10 (MQTT) for signaling
    // mqtt://canvasl/webrtc/${peerId}/signal
  }
}
```

## Complete User Interaction Flow

### Consciousness + Camera + Audio + XR

Full pipeline integrating all W3C media APIs:

```typescript
class ImmersiveConsciousnessSession {
  async startSession(): Promise<void> {
    // 1. Request all permissions
    const permissions = await permissionManager.requestAll();
    if (!permissions.camera || !permissions.microphone) {
      throw new Error('Camera/microphone required');
    }

    // 2. Start camera feed
    const cameraStream = await deviceManager.captureDevice('camera-front');

    // 3. Start microphone with audio processing
    const audioProcessor = new AudioProcessor();
    await audioProcessor.loadPAdicProcessor();
    const audioStream = await deviceManager.captureDevice('microphone');

    // 4. Start XR session (AR or VR)
    const xrManager = new WebXRManager();
    const xrSession = await xrManager.startAR();

    // 5. Load consciousness state
    const consciousnessState = await loadConsciousnessState();

    // 6. Render qualia field in AR space
    const qualiaVisualization = await renderQualiaFieldAR(
      consciousnessState,
      xrSession
    );

    // 7. Audio feedback from consciousness
    const qualiaAudio = audioProcessor.synthesizeQualiaField(consciousnessState);
    audioProcessor.playBuffer(qualiaAudio);

    // 8. Record entire session
    const recorder = new MediaRecorderManager();
    const compositeStream = new MediaStream([
      ...cameraStream.getTracks(),
      ...audioStream.getTracks()
    ]);
    recorder.record(compositeStream);

    // 9. Real-time consciousness updates from voice
    audioProcessor.on('p-adic-signature', async (signature) => {
      const newState = await updateConsciousnessFromVoice(signature);
      qualiaVisualization.update(newState);
    });

    // 10. Hand tracking for interaction
    xrSession.addEventListener('selectstart', (event) => {
      const inputSource = event.inputSource;
      if (inputSource.hand) {
        // Pinch gesture detected → capture action
        const action = detectHandGesture(inputSource.hand);
        updateConsciousnessFromAction(action);
      }
    });

    // Session runs until user exits XR
    xrSession.addEventListener('end', async () => {
      recorder.stop();
      const sessionRecording = await new Promise<Blob>(resolve => {
        recorder.once('recording-complete', resolve);
      });

      // Save to CBS
      const cid = await saveToCBS(sessionRecording);
      console.log('Session saved:', cid);
    });
  }
}
```

## Implementation Roadmap

### Phase 1: Core Media Device Management ✅ Foundation

**Goal**: Enumerate, capture, and release camera/microphone/screen

**Components**:
1. MediaDeviceManager class
2. Permission request flow
3. Device enumeration UI
4. Stream capture/release API

**Deliverable**: `scheme/media/devices.scm` + `services/media-api/devices.ts`

### Phase 2: Screen Capture + Recording 🚧 Video Capture

**Goal**: Record screen, camera, or composite feeds

**Components**:
1. ScreenCaptureManager
2. MediaRecorderManager
3. Codec selection and format conversion
4. CBS integration for storage

**Deliverable**: `services/media-api/recording.ts`

### Phase 3: Web Audio Processing 🎯 Audio Substrate

**Goal**: Real-time audio analysis and synthesis

**Components**:
1. AudioProcessor with WebAudio graph
2. p-adic AudioWorklet processor
3. Spatial audio (HRTF panning)
4. Consciousness → audio synthesis

**Deliverable**: `scheme/audio/processor.scm` + `public/p-adic-processor.js`

### Phase 4: WebXR Integration 🎯 Immersive Computing

**Goal**: AR/VR sessions with CanvasL metaverse

**Components**:
1. WebXRManager (AR/VR session control)
2. Hand tracking renderer
3. Hit testing for AR placement
4. GPS → XR world coordinate mapping

**Deliverable**: `scheme/xr/session.scm` + integration with Babylon.js

### Phase 5: Hybrid Video Generation 🌟 Complete Pipeline

**Goal**: Composite videos mixing 3D renders + camera + screen + audio

**Components**:
1. Multi-source compositor
2. Real-time effects (filters, overlays)
3. Template-driven video specs
4. End-to-end encoding pipeline

**Deliverable**: Extension of `43-3D-Template-Video.md` system

### Phase 6: Biometric + WebRTC + MQTT 🌐 Full Stack

**Goal**: Complete interaction layer with peer sync

**Components**:
1. WebAuthn + camera recording
2. WebRTC video calls with A9 automaton
3. MQTT signaling via A10 automaton
4. Distributed consciousness visualization

**Deliverable**: Integration of A7, A9, A10 with media APIs

## Categorical Semantics

The complete media interaction pipeline exhibits layered adjunctions:

```
Physical Devices ⊣ Digital Streams (hardware ↔ software)
       ↓
  MediaStream ⊣ MediaRecorder (real-time ↔ recorded)
       ↓
  AudioContext ⊣ AudioWorklet (declarative ↔ imperative)
       ↓
  XRSession ⊣ XRFrame (session state ↔ frame rendering)
       ↓
  Template ⊣ Rendering (specification ↔ observation)
```

Each adjunction preserves structure while changing representation:
- Devices → Streams: Continuous signal sampling
- Streams → Recorder: Temporal buffering and encoding
- AudioContext → Worklet: Sample-by-sample processing
- XRSession → Frame: Per-frame spatial rendering
- Template → Rendering: Specification evaluation

The composition of all right adjoints `Render ∘ Frame ∘ Worklet ∘ Recorder ∘ Stream` is the **grand observer**: it takes physical reality (devices, sensors) and produces digital artifacts (videos, audio files, XR scenes).

## References

- [3D Template Video](43-3D-Template-Video.md) - Video generation framework
- [Babylon Integration](../../docs/research/babylon-integration.md) - 3D rendering engine
- [WebRTC Messenger](../../automaton-evolutions/files/a9-webrtc-messenger.canvasl) - A9 automaton
- [WebAuthn Oracle](../../automaton-evolutions/files/a7-webauthn-oracle.canvasl) - A7 automaton
- [MQTT Herald](../../automaton-evolutions/files/a10-mqtt-herald.canvasl) - A10 automaton
- [Protocol Handlers](../../docs/PROTOCOL_HANDLERS.md) - canvasl://, webrtc://, mqtt:// URLs
- [Computational Physics](41-Computational-Physics.md) - Consciousness substrate

---

**Status**: Specification complete. W3C APIs are browser-native; implementation requires frontend TypeScript integration with existing Scheme substrate.

**Browser Compatibility**:
- getUserMedia: ✅ All modern browsers
- getDisplayMedia: ✅ Chrome, Firefox, Safari (iOS 15.5+)
- MediaRecorder: ✅ Chrome, Firefox, Safari (limited codec support)
- Web Audio: ✅ Universal support
- WebXR: ⚠️ Chrome (AR+VR), Firefox (VR only), Safari (AR only via Quick Look)

**Next steps**:
1. Implement MediaDeviceManager with permission flow
2. Add screen capture + MediaRecorder integration
3. Create AudioWorklet for p-adic processing
4. Integrate WebXR with Babylon.js renderer
5. Build composite video generator (3D + camera + screen)
6. Connect to A7/A9/A10 automatons

**Timeline**: Phase 1-2 (1 week), Phase 3 (1 week), Phase 4 (2 weeks), Phase 5-6 (2 weeks) = 6 weeks to complete media interaction layer.
