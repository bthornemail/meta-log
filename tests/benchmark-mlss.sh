#!/bin/bash
# MLSS Benchmark Suite
# Meta-Log Substrate System - Performance Benchmarks
# Copyright (C) 2025 Meta-Log Research Group

set -e

BENCHMARK_LOG="tests/BENCHMARK-RESULTS.md"
TIMEFORMAT="%R"

echo "=========================================="
echo "MLSS Benchmark Suite"
echo "=========================================="
echo ""
echo "Starting benchmarks at $(date)"
echo ""

# Initialize benchmark log
cat > "$BENCHMARK_LOG" << 'EOF'
# MLSS Benchmark Results
**Meta-Log Substrate System Performance Benchmarks**

Generated: $(date)

## Benchmark Methodology

All benchmarks run on:
- System: $(uname -a)
- Guile Version: $(guile --version | head -1)
- Python Version: $(python3 --version)

Times are measured in seconds (wall-clock time).

---

EOF

# Helper function to run benchmark
run_benchmark() {
    local name="$1"
    local command="$2"
    local iterations="${3:-10}"
    
    echo "Benchmarking: $name"
    echo "  Iterations: $iterations"
    
    # Check if bc is available, otherwise use simpler timing
    if command -v bc >/dev/null 2>&1; then
        local total_time=0
        local min_time=999999
        local max_time=0
        
        for i in $(seq 1 $iterations); do
            local start_time=$(date +%s.%N)
            eval "$command" > /dev/null 2>&1
            local end_time=$(date +%s.%N)
            local elapsed=$(echo "$end_time - $start_time" | bc)
            
            if [ -n "$elapsed" ] && [ "$elapsed" != "0" ]; then
                total_time=$(echo "$total_time + $elapsed" | bc)
                min_time=$(echo "if ($elapsed < $min_time) $elapsed else $min_time" | bc)
                max_time=$(echo "if ($elapsed > $max_time) $elapsed else $max_time" | bc)
            fi
        done
        
        if [ "$total_time" != "0" ]; then
            local avg_time=$(echo "scale=4; $total_time / $iterations" | bc)
            
            echo "  Average: ${avg_time}s"
            echo "  Min: ${min_time}s"
            echo "  Max: ${max_time}s"
            echo ""
            
            # Append to log
            cat >> "$BENCHMARK_LOG" << EOF
### $name

- **Iterations**: $iterations
- **Average Time**: ${avg_time}s
- **Min Time**: ${min_time}s
- **Max Time**: ${max_time}s
- **Total Time**: ${total_time}s

EOF
        else
            echo "  Warning: Could not measure timing accurately"
            echo ""
        fi
    else
        # Fallback: use time command
        echo "  (Using time command - install 'bc' for detailed stats)"
        local total_output=$(time (for i in $(seq 1 $iterations); do eval "$command" > /dev/null 2>&1; done) 2>&1)
        local total_time=$(echo "$total_output" | grep real | awk '{print $2}')
        echo "  Total time: ${total_time}"
        echo ""
        
        cat >> "$BENCHMARK_LOG" << EOF
### $name

- **Iterations**: $iterations
- **Total Time**: ${total_time}
- **Note**: Install 'bc' for detailed statistics

EOF
    fi
}

# Phase 1: Foundation Benchmarks
echo "=== Phase 1: Foundation ==="
run_benchmark "Substrate Memory Creation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (substrate-create-memory #u8(1 2 3 4) '\''((test . \"benchmark\"))'\'')'" \
    100

run_benchmark "CBS Creation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (make-cbs #u8(1 2 3 4 5 6 7 8) '\''((encoding . \"raw\"))'\'')'" \
    100

run_benchmark "Content Hash Computation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (content-hash #u8(1 2 3 4 5 6 7 8) '\''((test . \"hash\"))'\'')'" \
    100

# Phase 2: Waveform Benchmarks
echo "=== Phase 2: Waveform & Geometric ==="
run_benchmark "Waveform Creation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (make-waveform '\''(1.0 2.0 3.0 4.0) '\''() 44100)'\''" \
    50

run_benchmark "WDL Compilation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (wdl-compile '\''(sine (freq 440) (amp 0.5))'\'')'" \
    50

# Phase 3: Q* Benchmarks
echo "=== Phase 3: Q* Optimality Engine ==="
run_benchmark "Q* State Creation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (make-qstar-state '\''((x . 0) (y . 0)) '\''((goal-x . 10) (goal-y . 10))'\'')'" \
    100

run_benchmark "Q* Evaluation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (let ((state (make-qstar-state '\''((x . 0) (y . 0)) '\''((goal-x . 10) (goal-y . 10))'\'')) (action (make-qstar-action '\''move-right'\'' '\''((dx . 1) (dy . 0))'\''))) (qstar-evaluate state action))'" \
    50

# Phase 4: Vision Benchmarks
echo "=== Phase 4: Computer Vision ==="
run_benchmark "Image Creation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (make-image 100 100 '\''((1 2 3) (4 5 6) (7 8 9))'\'')'" \
    50

run_benchmark "Image to CBS Conversion" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (let ((img (make-image 50 50 '\''((1 2 3) (4 5 6) (7 8 9))'\''))) (image-to-cbs img))'" \
    50

# Phase 5: Consciousness Benchmarks
echo "=== Phase 5: Consciousness Framework ==="
run_benchmark "Conscious State Creation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (make-conscious-state 5.0 0.7 0.8)'\''" \
    100

run_benchmark "Qualia Emergence" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (emerge-qualia 5.0 0.7 0.8 0.3)'\''" \
    100

run_benchmark "Consciousness Metrics" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (let ((state1 (make-conscious-state 5.0 0.7 0.8)) (state2 (make-conscious-state 10.0 0.8 0.9))) (collect-metrics state2 state1 #f))'" \
    50

# Phase 6: Physics Benchmarks
echo "=== Phase 6: Computational Physics ==="
run_benchmark "Quantum State Creation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (make-quantum-state 2 '\''(0.707 0.707 0.0 0.0))'\'')'" \
    100

run_benchmark "Einstein Equations" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (einstein-equations '\''((1.0 0.0 0.0 0.0) (0.0 1.0 0.0 0.0) (0.0 0.0 1.0 0.0) (0.0 0.0 0.0 1.0))'\'')'" \
    50

run_benchmark "Field Configuration Creation" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (make-field-configuration '\''scalar '\''(1.0 2.0 3.0 4.0) 0.1)'\''" \
    100

# Integration Benchmarks
echo "=== Integration Benchmarks ==="
run_benchmark "Cross-Domain Mapping (Binary → Waveform)" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\") (let* ((cbs (make-cbs #u8(1 2 3 4) '\''((encoding . \"raw\"))'\'')) (mem (substrate-create-memory (list-ref cbs 2) '\''((content-type . \"cbs\"))'\'')) (uri (list-ref mem 1))) (binary-to-waveform uri))'" \
    20

run_benchmark "Full MLSS Engine Load" \
    "guile -c '(load \"scheme/r5rs-canvas-engine.scm\")'" \
    10

# Summary
echo "=========================================="
echo "Benchmark Suite Complete"
echo "=========================================="
echo ""
echo "Results saved to: $BENCHMARK_LOG"
echo ""

# Append summary to log
cat >> "$BENCHMARK_LOG" << EOF

---

## Summary

Benchmark suite completed at $(date)

Total benchmarks run: $(grep -c "^###" "$BENCHMARK_LOG" || echo "0")

EOF

