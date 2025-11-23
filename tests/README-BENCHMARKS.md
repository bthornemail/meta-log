# MLSS Benchmarking and Demo Guide

## Quick Start

### Run Full Benchmark Suite
```bash
./tests/benchmark-mlss.sh
```

This will:
- Run performance benchmarks for all 6 phases
- Generate detailed timing statistics
- Save results to `tests/BENCHMARK-RESULTS.md`

### Run Interactive Demo
```bash
./tests/demo-mlss.sh
```

This will:
- Demonstrate all MLSS capabilities interactively
- Show examples from each phase
- Walk through the complete system workflow

## Benchmark Details

### What Gets Benchmarked

**Phase 1: Foundation**
- Substrate memory object creation
- CBS (Canonical Binary Substrate) creation
- Content hash computation

**Phase 2: Waveform & Geometric**
- Waveform object creation
- WDL (Waveform Description Language) compilation

**Phase 3: Q* Optimality Engine**
- Q* state creation
- Q* action evaluation

**Phase 4: Computer Vision**
- Image object creation
- Image to CBS conversion

**Phase 5: Consciousness Framework**
- Conscious state creation
- Qualia emergence computation
- Consciousness metrics collection

**Phase 6: Computational Physics**
- Quantum state creation
- Einstein field equations computation
- Field configuration creation

**Integration**
- Cross-domain mappings
- Full engine load time

### Benchmark Methodology

- Each benchmark runs multiple iterations (typically 10-100)
- Reports: average, min, max, and total time
- Times measured in seconds (wall-clock)
- Results saved to markdown log file

## Demo Details

### Demo Flow

1. **Foundation Demo**
   - Creates memory objects
   - Demonstrates CBS encoding
   - Shows content addressing

2. **Waveform Demo**
   - Compiles WDL specifications
   - Creates waveform objects
   - Shows sample generation

3. **Q* Demo**
   - Solves pathfinding problem
   - Evaluates action costs
   - Selects optimal actions

4. **Vision Demo**
   - Creates test images
   - Converts to CBS format
   - Extracts edge features

5. **Consciousness Demo**
   - Simulates conscious state evolution
   - Shows forward/backward propagation
   - Demonstrates qualia emergence

6. **Physics Demo**
   - Creates quantum states
   - Computes field equations
   - Shows field configurations

## Interpreting Results

### Performance Expectations

- **Fast operations** (< 0.01s): Memory creation, state initialization
- **Medium operations** (0.01-0.1s): Evaluations, transformations
- **Slow operations** (> 0.1s): Complex computations, full engine loads

### Optimization Opportunities

If benchmarks show slow performance:
1. Check system load
2. Verify Guile version (3.0+ recommended)
3. Review implementation for bottlenecks
4. Consider caching for repeated operations

## Troubleshooting

### Benchmark Script Issues

**Problem**: `bc: command not found`
**Solution**: Install bc: `sudo apt-get install bc` or `sudo yum install bc`

**Problem**: Timeout errors
**Solution**: Increase timeout or reduce iterations in script

### Demo Issues

**Problem**: Demo hangs waiting for input
**Solution**: Use non-interactive mode or provide input via stdin

**Problem**: Module load errors
**Solution**: Ensure all Scheme files are in correct locations

## Advanced Usage

### Custom Benchmarks

Edit `tests/benchmark-mlss.sh` to add custom benchmarks:

```bash
run_benchmark "My Custom Benchmark" \
    "guile -c 'your-command-here'" \
    50  # iterations
```

### Automated Benchmarking

Run benchmarks in CI/CD:

```bash
./tests/benchmark-mlss.sh > benchmark-output.txt 2>&1
```

### Performance Profiling

Use Guile's profiling:

```bash
guile --debug -e '(load "scheme/r5rs-canvas-engine.scm")' your-test.scm
```

## Results Format

Benchmark results are saved in markdown format:

```markdown
### Benchmark Name

- **Iterations**: 100
- **Average Time**: 0.005s
- **Min Time**: 0.004s
- **Max Time**: 0.007s
- **Total Time**: 0.5s
```

## Contact

For questions or issues with benchmarking:
- Check `dev-docs/INTEGRATION-MLSS.md` for architecture details
- Review test files in `scheme/**/*.test.scm`
- See integration tests in `tests/test-*-workflow.sh`

