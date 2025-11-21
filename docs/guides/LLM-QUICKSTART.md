# LLM Quick Start Guide

Get started with meta-log's LLM-powered natural language queries in 5 minutes.

## Step 1: Choose Your Backend

### Option A: Ollama (Local, Free, Private) - Recommended for Desktop/Tablet

```bash
# Install Ollama
curl -fsSL https://ollama.com/install.sh | sh

# Start Ollama
ollama serve

# Pull a small model (in another terminal)
ollama pull gemma2:2b
```

```elisp
;; In Emacs
(require 'meta-log-llm)
(meta-log-llm-openai-use-ollama "gemma2:2b")
```

### Option B: Claude API (Cloud, Accurate) - Recommended for Production

```elisp
(require 'meta-log-llm)
(require 'meta-log-llm-anthropic)
(setq meta-log-llm-anthropic-api-key "sk-ant-YOUR-KEY")
(setq meta-log-llm-backend 'hybrid)  ; Use local when possible, Claude for complex
```

### Option C: Hybrid (Best of Both)

```elisp
(require 'meta-log-llm)

;; Setup local embeddings for fast queries
(require 'meta-log-llm-tflite)
(meta-log-llm-tflite-setup)  ; Installs sentence-transformers

;; Setup Claude for complex queries
(require 'meta-log-llm-anthropic)
(setq meta-log-llm-anthropic-api-key "sk-ant-YOUR-KEY")

;; Use hybrid mode
(setq meta-log-llm-backend 'hybrid)
(setq meta-log-llm-confidence-threshold 0.7)
```

## Step 2: Initialize

```elisp
(require 'meta-log)
(require 'meta-log-llm)

(meta-log-initialize)
(meta-log-llm-initialize)
```

## Step 3: Start Querying!

### Basic Query
```elisp
(meta-log-llm-query "church encoding")
```

**Result:**
```
Translated Query: kg_node(?Id, concept, ?Label), matches(?Label, "church|lambda|encoding")
Expanded Concepts: lambda calculus, scott encoding, functional programming
Dimension: 3D
Confidence: 0.95
Source: remote
```

### Vague Concept Query
```elisp
(meta-log-llm-query "that functional programming thing with lambdas")
```

**It figures out you mean "church encoding"!**

### Find Related Concepts
```elisp
(meta-log-llm-expand-concept "merkle tree")
→ ("hash tree" "blockchain" "verification" "cryptography" ...)
```

### Classify Dimension
```elisp
(meta-log-llm-classify-dimension "network topology")
→ (:dimension "4D" :confidence 0.92 :reasoning "Network is 4D distributed systems")
```

## Step 4: Let It Learn

The system automatically learns from your usage:

```elisp
;; After a few queries, check what it learned
(meta-log-llm-cache-show-top-queries)
```

Shows:
```
  1. [10 uses] church encoding
  2. [ 8 uses] lambda calculus
  3. [ 5 uses] merkle tree verification
```

### Add Personal Vocabulary

```elisp
(meta-log-llm-add-vocabulary "my encoding stuff" "church encoding")

;; Now this works:
(meta-log-llm-query "my encoding stuff")
→ Translates to "church encoding"
```

### Correct Wrong Translations

```elisp
(meta-log-llm-learning-correct-translation
 "network things"
 "kg_dimension(?Id, \"4D\")")

;; System remembers the correction
```

## Step 5: Integration with Knowledge Graph

### Load Your Documents
```elisp
(require 'meta-log-knowledge-graph)
(meta-log-kg-ingest-directory "/path/to/your/docs")
```

### Query with Natural Language
```elisp
;; Vague query
(let ((result (meta-log-llm-query "functional programming concepts")))
  (let ((prolog-query (meta-log-llm-query-result-translated-query result)))
    ;; Execute on knowledge graph
    (meta-log-kg-query-prolog prolog-query)))
```

### Enhanced Search
```elisp
(defun my-smart-search (query)
  "Search knowledge graph with LLM translation."
  (interactive "sSearch: ")

  (let* ((llm-result (meta-log-llm-query query))
         (concepts (meta-log-llm-query-result-expanded-concepts llm-result))
         (dimension (meta-log-llm-query-result-dimension llm-result)))

    (message "Searching for: %s (dimension: %s)" concepts dimension)

    ;; Search with expanded concepts
    (meta-log-kg-query (mapconcat #'identity concepts " OR "))))
```

## Configuration Tips

### For Phone/Termux (Resource Constrained)

```elisp
;; TFLite only - fast and lightweight
(setq meta-log-llm-backend 'tflite)
(meta-log-llm-tflite-setup)

;; OR use API (requires internet)
(setq meta-log-llm-backend 'anthropic)
(setq meta-log-llm-anthropic-api-key "...")
```

### For Desktop (Powerful)

```elisp
;; Hybrid with Ollama
(setq meta-log-llm-backend 'hybrid)
(meta-log-llm-openai-use-ollama "gemma2:2b")
(meta-log-llm-tflite-setup)
```

### For Production (Accurate)

```elisp
;; Hybrid with Claude
(setq meta-log-llm-backend 'hybrid)
(setq meta-log-llm-anthropic-api-key "...")
(meta-log-llm-tflite-setup)  ; Fast queries stay local
```

## Common Workflows

### 1. Research Assistant

```elisp
(defun my-research (topic)
  "Research TOPIC using LLM + knowledge graph."
  (interactive "sTopic: ")

  ;; Expand concept
  (let ((related (meta-log-llm-expand-concept topic)))
    (message "Researching: %s" topic)
    (message "Related concepts: %s" related)

    ;; Search knowledge graph
    (meta-log-kg-query (mapconcat #'identity (cons topic related) " OR "))))
```

### 2. Code Helper

```elisp
(defun my-find-pattern (description)
  "Find code pattern from DESCRIPTION."
  (interactive "sDescribe pattern: ")

  (let* ((result (meta-log-llm-query description))
         (dimension (meta-log-llm-query-result-dimension result)))

    ;; Filter by code dimension (usually 2D or 3D)
    (when (member dimension '("2D" "3D"))
      (meta-log-kg-query description))))
```

### 3. Document Finder

```elisp
(defun my-find-docs (vague-query)
  "Find documents matching VAGUE-QUERY."
  (interactive "sWhat are you looking for? ")

  (let* ((llm-result (meta-log-llm-query vague-query))
         (prolog (meta-log-llm-query-result-translated-query llm-result)))

    (meta-log-kg-query-documents prolog)))
```

## Monitoring

### Check Statistics
```elisp
(meta-log-llm-stats)  ; Overall stats
(meta-log-llm-cache-show-stats)  ; Cache performance
(meta-log-llm-learning-stats)  ; What system learned
```

### Test Backends
```elisp
(meta-log-llm-openai-test)  ; Test Ollama/OpenAI
(meta-log-llm-anthropic-test)  ; Test Claude
(meta-log-llm-tflite-test)  ; Test local embeddings
```

## Keyboard Shortcuts (Optional)

Add to your Emacs config:

```elisp
(global-set-key (kbd "C-c q") 'meta-log-llm-query)
(global-set-key (kbd "C-c e") 'meta-log-llm-expand-concept)
(global-set-key (kbd "C-c s") 'meta-log-llm-stats)
```

## Next Steps

1. **Read** [docs/architecture/LLM-INTEGRATION.md](../architecture/LLM-INTEGRATION.md) for deep dive
2. **Explore** [docs/api/LLM-API.md](../api/LLM-API.md) for full API reference
3. **Integrate** with your workflow

## Troubleshooting

### "sentence-transformers not found"
```elisp
(meta-log-llm-tflite-setup)  ; Auto-installs
```

### "Ollama connection refused"
```bash
ollama serve  # Start Ollama server
```

### "API key invalid"
```elisp
;; Set correct key
(setq meta-log-llm-anthropic-api-key "sk-ant-...")
```

### "Low confidence results"
```elisp
;; Use more powerful backend
(setq meta-log-llm-backend 'anthropic)

;; Or lower threshold
(setq meta-log-llm-confidence-threshold 0.5)
```

## Summary

You now have:
- ✅ Natural language query translation
- ✅ Concept expansion
- ✅ Automatic learning
- ✅ Personal vocabulary
- ✅ Cache for speed
- ✅ Multiple backend options

**Start querying with indefinite concepts!**
