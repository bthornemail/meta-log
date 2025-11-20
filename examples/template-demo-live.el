;;; template-demo-live.el --- Live template discovery demonstration

;; This script demonstrates template discovery working end-to-end.

(message "\n========================================")
(message "META-LOG TEMPLATE DISCOVERY DEMO")
(message "========================================\n")

;; Step 1: Load path setup
(message "[1/7] Setting up load-path...")
(add-to-list 'load-path "/data/data/com.termux/files/home/github/meta-log")
(message "✓ Load-path configured\n")

;; Step 2: Load dependencies
(message "[2/7] Loading dependencies...")
(require 'meta-log-core)
(require 'meta-log-wordnet)
(require 'meta-log-canvas-api)
(message "✓ WordNet module loaded")
(message "✓ Canvas API module loaded\n")

;; Step 3: Test WordNet keyword extraction
(message "[3/7] Testing WordNet semantic analysis...")
(let ((text "peer identity management with blockchain consensus"))
  (message "Input text: \"%s\"" text)
  (let ((keywords (meta-log-wordnet-extract-keywords text)))
    (message "\nExtracted keywords:")
    (dolist (keyword keywords)
      (message "  • %s" (plist-get keyword :word))
      (when (plist-get keyword :dimension)
        (message "    ├─ Dimension: %s" (plist-get keyword :dimension)))
      (when (plist-get keyword :semantic-field)
        (message "    └─ Semantic field: %s" (plist-get keyword :semantic-field)))))

  ;; Test dimension mapping
  (let ((dimension (meta-log-wordnet-map-to-dimension text)))
    (message "\nMapped to dimension: %s" dimension))

  ;; Test semantic field
  (let ((field (meta-log-wordnet-semantic-field text)))
    (message "Semantic field: %s\n" field)))

;; Step 4: Test Canvas API mapping
(message "[4/7] Testing Canvas API mapping...")
(let ((keywords '("peer" "identity" "location" "camera")))
  (message "Mapping keywords to Canvas APIs:")
  (dolist (keyword keywords)
    (let ((mapping (meta-log-canvas-api-map-keyword keyword)))
      (if mapping
          (message "  • \"%s\" → navigator.%s.%s()"
                   keyword (car mapping) (cdr mapping))
        (message "  • \"%s\" → (no mapping)" keyword))))
  (message ""))

;; Step 5: Test semantic similarity
(message "[5/7] Testing semantic similarity...")
(let ((text1 "peer identity management")
      (text2 "node self administration"))
  (message "Comparing:")
  (message "  Text 1: \"%s\"" text1)
  (message "  Text 2: \"%s\"" text2)
  (let ((similarity (meta-log-wordnet-semantic-similarity text1 text2)))
    (message "  Similarity score: %.2f (%.0f%% match)\n"
             similarity (* similarity 100))))

;; Step 6: Create sample template directory
(message "[6/7] Creating sample template for discovery...")
(let ((template-dir (expand-file-name "~/.emacs.d/meta-log/templates")))
  (unless (file-directory-p template-dir)
    (make-directory template-dir t)
    (message "✓ Created directory: %s" template-dir))

  ;; Create a sample template file
  (let ((template-file (expand-file-name "peer-identity.org" template-dir)))
    (with-temp-file template-file
      (insert "* Peer Identity Management\n")
      (insert ":PROPERTIES:\n")
      (insert ":CANVASL_CID: template-peer-001\n")
      (insert ":CANVASL_DIMENSION: 5D\n")
      (insert ":CANVASL_SEMANTIC_FIELD: network-identity\n")
      (insert ":END:\n\n")
      (insert "Template for managing peer identities with cryptographic keys.\n\n")
      (insert "** Keywords\n")
      (insert "- peer\n")
      (insert "- identity\n")
      (insert "- crypto\n")
      (insert "- WebAuthn\n\n")
      (insert "** Canvas API Mappings\n")
      (insert "- webauthn.create()\n")
      (insert "- indexeddb.put()\n")
      (insert "- crypto.subtle.generateKey()\n"))
    (message "✓ Created sample template: %s\n" template-file)))

;; Step 7: Build and display a complete CanvasL template
(message "[7/7] Generating complete CanvasL template...")
(let* ((description "peer identity management with WebAuthn")
       (keywords (meta-log-wordnet-extract-keywords description))
       (keyword-words (mapcar (lambda (k) (plist-get k :word)) keywords))
       (dimension (meta-log-wordnet-map-to-dimension description))
       (semantic-field (meta-log-wordnet-semantic-field description)))

  (message "\nGenerating template from:")
  (message "  Description: \"%s\"" description)
  (message "  Keywords: %S" keyword-words)
  (message "  Dimension: %s" dimension)
  (message "  Semantic field: %s" semantic-field)

  ;; Build Canvas API mappings
  (let ((mappings '()))
    (dolist (keyword keyword-words)
      (let ((mapping (meta-log-canvas-api-map-keyword keyword)))
        (when mapping
          (push (list :keyword keyword
                      :api (car mapping)
                      :method (cdr mapping))
                mappings))))

    ;; Generate CanvasL content
    (let ((canvasl-content
           (concat
            "@version 1.0\n"
            "@schema canvasl\n"
            (format "@dimension %s\n" (or dimension "5D"))
            (format "@generated %s\n\n" (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
            "* Peer Identity Management With Webauthn\n"
            ":PROPERTIES:\n"
            (format ":CANVASL_CID: template-%d\n" (random 1000000))
            (format ":CANVASL_DIMENSION: %s\n" (or dimension "5D"))
            (format ":CANVASL_SEMANTIC_FIELD: %s\n" (or semantic-field "network-identity"))
            ":END:\n\n"
            (format "# %s\n\n" description)
            (format "%s\n\n" description)
            "## Canvas API Mappings\n\n"
            (mapconcat
             (lambda (mapping)
               (format "- **%s**: navigator.%s.%s()"
                       (plist-get mapping :keyword)
                       (plist-get mapping :api)
                       (plist-get mapping :method)))
             (nreverse mappings)
             "\n"))))

      (message "\n========================================")
      (message "GENERATED CANVASL TEMPLATE")
      (message "========================================\n")
      (message "%s" canvasl-content)

      ;; Save to file
      (let ((output-file "/data/data/com.termux/files/home/github/meta-log/examples/demo-generated-template.canvasl"))
        (with-temp-file output-file
          (insert canvasl-content))
        (message "\n✓ Template saved to: %s" output-file)))))

(message "\n========================================")
(message "DEMO COMPLETE!")
(message "========================================\n")

(message "What just happened:")
(message "1. ✓ Loaded WordNet and Canvas API modules")
(message "2. ✓ Extracted keywords from natural language")
(message "3. ✓ Mapped keywords to dimensions (0D-7D)")
(message "4. ✓ Mapped keywords to Web Canvas APIs")
(message "5. ✓ Calculated semantic similarity")
(message "6. ✓ Created sample template file")
(message "7. ✓ Generated complete CanvasL template")
(message "\nNext steps:")
(message "- View generated template: examples/demo-generated-template.canvasl")
(message "- View sample template: ~/.emacs.d/meta-log/templates/peer-identity.org")
(message "- Try: M-x load-file RET examples/template-demo-live.el RET\n")

(provide 'template-demo-live)

;;; template-demo-live.el ends here
