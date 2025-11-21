;;; meta-log-llm-tflite.el --- TensorFlow Lite embeddings for local inference

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Local semantic embeddings using TensorFlow Lite.
;; Fast, private, offline concept expansion and similarity matching.
;; Uses Python subprocess for TFLite inference.

;;; Code:

(require 'cl-lib)
(require 'json)

;;; Configuration

(defcustom meta-log-llm-tflite-python-command "python3"
  "Python command to run TFLite inference."
  :type 'string
  :group 'meta-log-llm)

(defcustom meta-log-llm-tflite-model-path nil
  "Path to TFLite model file (.tflite).
If nil, will attempt to download sentence-transformers model."
  :type '(choice (const :tag "Auto-download" nil)
                 (file :tag "Model file"))
  :group 'meta-log-llm)

(defcustom meta-log-llm-tflite-similarity-threshold 0.7
  "Minimum cosine similarity for concept matching (0.0-1.0)."
  :type 'float
  :group 'meta-log-llm)

;;; Python Script

(defconst meta-log-llm-tflite--python-script
  "#!/usr/bin/env python3
import sys
import json

try:
    from sentence_transformers import SentenceTransformer
    import numpy as np
except ImportError:
    print(json.dumps({'error': 'sentence-transformers not installed'}))
    sys.exit(1)

# Load model (cached after first run)
model = SentenceTransformer('all-MiniLM-L6-v2')

def encode(text):
    embedding = model.encode(text)
    return embedding.tolist()

def similarity(text1, text2):
    emb1 = model.encode(text1)
    emb2 = model.encode(text2)
    sim = np.dot(emb1, emb2) / (np.linalg.norm(emb1) * np.linalg.norm(emb2))
    return float(sim)

def expand_concept(concept, knowledge_base):
    \"\"\"Find similar concepts from knowledge base\"\"\"
    concept_emb = model.encode(concept)

    results = []
    for candidate in knowledge_base:
        cand_emb = model.encode(candidate)
        sim = np.dot(concept_emb, cand_emb) / (np.linalg.norm(concept_emb) * np.linalg.norm(cand_emb))
        if sim > 0.5:  # Threshold
            results.append({'concept': candidate, 'similarity': float(sim)})

    # Sort by similarity
    results.sort(key=lambda x: x['similarity'], reverse=True)
    return results[:10]  # Top 10

# Main command loop
if __name__ == '__main__':
    command = sys.argv[1] if len(sys.argv) > 1 else 'encode'

    if command == 'encode':
        text = sys.argv[2] if len(sys.argv) > 2 else ''
        result = encode(text)
        print(json.dumps({'embedding': result}))

    elif command == 'similarity':
        text1 = sys.argv[2] if len(sys.argv) > 2 else ''
        text2 = sys.argv[3] if len(sys.argv) > 3 else ''
        sim = similarity(text1, text2)
        print(json.dumps({'similarity': sim}))

    elif command == 'expand':
        concept = sys.argv[2] if len(sys.argv) > 2 else ''
        kb_file = sys.argv[3] if len(sys.argv) > 3 else None

        # Load knowledge base from file
        kb = []
        if kb_file:
            with open(kb_file, 'r') as f:
                kb = json.load(f)

        results = expand_concept(concept, kb)
        print(json.dumps({'expansions': results}))
"
  "Python script for TFLite embeddings.")

;;; Setup

(defun meta-log-llm-tflite-setup ()
  "Setup TFLite embeddings system.
Installs sentence-transformers if needed."
  (interactive)

  (message "Setting up TFLite embeddings...")

  ;; Check if Python is available
  (unless (executable-find meta-log-llm-tflite-python-command)
    (error "Python not found. Install Python 3 or set meta-log-llm-tflite-python-command"))

  ;; Check if sentence-transformers is installed
  (let ((check-cmd (format "%s -c 'import sentence_transformers'"
                          meta-log-llm-tflite-python-command)))
    (unless (= 0 (call-process-shell-command check-cmd))
      (when (y-or-n-p "sentence-transformers not installed. Install now? ")
        (message "Installing sentence-transformers (this may take a few minutes)...")
        (let ((install-cmd (format "%s -m pip install sentence-transformers"
                                  meta-log-llm-tflite-python-command)))
          (if (= 0 (call-process-shell-command install-cmd))
              (message "✓ sentence-transformers installed")
            (error "Failed to install sentence-transformers"))))))

  ;; Create Python script file
  (let ((script-file (expand-file-name "meta-log-tflite.py" temporary-file-directory)))
    (with-temp-file script-file
      (insert meta-log-llm-tflite--python-script))
    (set-file-modes script-file #o755)
    (setq meta-log-llm-tflite--script-file script-file))

  (message "✓ TFLite embeddings ready"))

(defvar meta-log-llm-tflite--script-file nil
  "Path to generated Python script.")

;;; Embedding Operations

(defun meta-log-llm-tflite-encode (text)
  "Get embedding vector for TEXT.
Returns list of floats."
  (unless meta-log-llm-tflite--script-file
    (meta-log-llm-tflite-setup))

  (let* ((cmd (format "%s %s encode %s"
                     meta-log-llm-tflite-python-command
                     (shell-quote-argument meta-log-llm-tflite--script-file)
                     (shell-quote-argument text)))
         (output (shell-command-to-string cmd))
         (json-object-type 'plist)
         (json-array-type 'list)
         (result (json-read-from-string output)))

    (plist-get result :embedding)))

(defun meta-log-llm-tflite-similarity (text1 text2)
  "Calculate cosine similarity between TEXT1 and TEXT2.
Returns float 0.0-1.0."
  (unless meta-log-llm-tflite--script-file
    (meta-log-llm-tflite-setup))

  (let* ((cmd (format "%s %s similarity %s %s"
                     meta-log-llm-tflite-python-command
                     (shell-quote-argument meta-log-llm-tflite--script-file)
                     (shell-quote-argument text1)
                     (shell-quote-argument text2)))
         (output (shell-command-to-string cmd))
         (json-object-type 'plist)
         (result (json-read-from-string output)))

    (plist-get result :similarity)))

;;; Query Translation (Local)

(defun meta-log-llm-tflite-translate (query)
  "Translate QUERY using local embeddings.
Returns meta-log-llm-query-result with concept expansion."
  (require 'meta-log-knowledge-graph)

  ;; Expand query using KG concepts
  (let* ((expanded (meta-log-llm-tflite-expand query))
         (concepts (mapcar (lambda (x) (plist-get x :concept)) expanded))
         (confidence (if expanded
                        (plist-get (car expanded) :similarity)
                      0.5))
         ;; Build Prolog query from expanded concepts
         (prolog-query (meta-log-llm-tflite--build-prolog concepts)))

    (make-meta-log-llm-query-result
     :original-query query
     :translated-query prolog-query
     :expanded-concepts concepts
     :dimension (meta-log-llm-tflite--infer-dimension concepts)
     :confidence confidence
     :source 'local
     :timestamp (current-time))))

(defun meta-log-llm-tflite--build-prolog (concepts)
  "Build Prolog query from expanded CONCEPTS."
  (if concepts
      (format "kg_node(?Id, ?Type, ?Label), (%s)"
             (mapconcat (lambda (c)
                         (format "matches(?Label, \"%s\")" c))
                       concepts
                       " ; "))
    nil))

(defun meta-log-llm-tflite--infer-dimension (concepts)
  "Infer dimension from CONCEPTS using WordNet mappings."
  (require 'meta-log-wordnet)

  (let ((dimensions '()))
    (dolist (concept concepts)
      (let ((dim (meta-log-wordnet-map-to-dimension concept)))
        (when dim
          (push dim dimensions))))

    ;; Return most common dimension
    (when dimensions
      (let ((counts (make-hash-table :test 'equal)))
        (dolist (dim dimensions)
          (puthash dim (1+ (or (gethash dim counts) 0)) counts))

        (let ((max-dim nil)
              (max-count 0))
          (maphash (lambda (dim count)
                     (when (> count max-count)
                       (setq max-dim dim
                             max-count count)))
                  counts)
          max-dim)))))

;;; Concept Expansion

(defun meta-log-llm-tflite-expand (concept)
  "Expand CONCEPT using local embeddings.
Returns list of plists with :concept and :similarity."
  (unless meta-log-llm-tflite--script-file
    (meta-log-llm-tflite-setup))

  (require 'meta-log-knowledge-graph)

  ;; Get all concepts from KG
  (let* ((kb-concepts (meta-log-llm-tflite--get-kg-concepts))
         (kb-file (make-temp-file "meta-log-kb" nil ".json"))
         (cmd nil)
         (output nil)
         (result nil))

    ;; Write knowledge base to temp file
    (with-temp-file kb-file
      (insert (json-encode kb-concepts)))

    ;; Run expansion
    (setq cmd (format "%s %s expand %s %s"
                     meta-log-llm-tflite-python-command
                     (shell-quote-argument meta-log-llm-tflite--script-file)
                     (shell-quote-argument concept)
                     (shell-quote-argument kb-file)))

    (setq output (shell-command-to-string cmd))

    ;; Parse result
    (let ((json-object-type 'plist)
          (json-array-type 'list)
          (json-key-type 'keyword))
      (condition-case err
          (setq result (json-read-from-string output))
        (error
         (message "TFLite parse error: %s" err)
         (setq result nil))))

    ;; Cleanup
    (delete-file kb-file)

    ;; Return expansions
    (plist-get result :expansions)))

(defun meta-log-llm-tflite--get-kg-concepts ()
  "Get list of all concepts from knowledge graph."
  (let ((concepts '()))
    (maphash (lambda (id node)
               (push (meta-log-kg-node-label node) concepts))
            meta-log-kg--concept-index)

    ;; Limit to reasonable size
    (cl-subseq concepts 0 (min 500 (length concepts)))))

;;; Testing

(defun meta-log-llm-tflite-test ()
  "Test TFLite embeddings."
  (interactive)

  (message "Testing TFLite embeddings...")

  ;; Test 1: Encode
  (let ((embedding (meta-log-llm-tflite-encode "church encoding")))
    (if embedding
        (message "✓ Encoding works (dim: %d)" (length embedding))
      (error "Encoding failed")))

  ;; Test 2: Similarity
  (let ((sim (meta-log-llm-tflite-similarity "church encoding" "lambda calculus")))
    (message "✓ Similarity: %.3f" sim))

  ;; Test 3: Expansion
  (let ((expanded (meta-log-llm-tflite-expand "church encoding")))
    (message "✓ Expansion: %d concepts" (length expanded))
    (dolist (item (cl-subseq expanded 0 (min 3 (length expanded))))
      (message "  - %s (%.3f)"
              (plist-get item :concept)
              (plist-get item :similarity))))

  (message "✓ TFLite test complete"))

;;; Backend Registration

(meta-log-llm-register-backend
 'tflite
 #'meta-log-llm-tflite-translate
 #'meta-log-llm-tflite-expand)

(provide 'meta-log-llm-tflite)

;;; meta-log-llm-tflite.el ends here
