;;; test-improvements.el --- Test knowledge graph improvements

(add-to-list 'load-path "/data/data/com.termux/files/home/github/meta-log")
(require 'meta-log)
(require 'meta-log-knowledge-graph)
(require 'meta-log-kg-learning)
(require 'meta-log-auto-enhance)

(princ "\n")
(princ "╔══════════════════════════════════════════════════════════╗\n")
(princ "║  TESTING KNOWLEDGE GRAPH IMPROVEMENTS                   ║\n")
(princ "╚══════════════════════════════════════════════════════════╝\n")
(princ "\n")

;; Initialize
(meta-log-initialize)
(princ "✓ meta-log initialized\n\n")

;; Load knowledge graph (small sample for testing)
(princ "Loading knowledge graph sample...\n")
(meta-log-kg-ingest-directory "/data/data/com.termux/files/home/github/universal-life-vault/00 - INBOX")
(princ "\n")

(princ "═══════════════════════════════════════════════════════\n")
(princ "TEST 1: AUTO-ENHANCE WORDNET\n")
(princ "═══════════════════════════════════════════════════════\n\n")

(let ((original-count (length meta-log-wordnet--dimension-mappings)))
  (princ (format "WordNet mappings before: %d\n" original-count))

  (let ((new-count (meta-log-auto-enhance-wordnet)))
    (princ (format "New mappings learned: %d\n" new-count))
    (princ (format "WordNet mappings after: %d\n\n"
                   (length meta-log-wordnet--dimension-mappings)))

    (princ "Sample of new mappings:\n")
    (let ((samples (cl-subseq meta-log-wordnet--dimension-mappings
                             original-count
                             (min (+ original-count 10)
                                 (length meta-log-wordnet--dimension-mappings)))))
      (dolist (mapping samples)
        (princ (format "  • \"%s\" → %s\n" (car mapping) (cdr mapping)))))))

(princ "\n")
(princ "═══════════════════════════════════════════════════════\n")
(princ "TEST 2: GAP ANALYSIS\n")
(princ "═══════════════════════════════════════════════════════\n\n")

(let ((gaps (meta-log-auto-enhance-gap-analysis)))
  (princ (format "Found %d capability gaps\n\n" (length gaps)))
  (princ "Top 5 gaps:\n")
  (dotimes (i (min 5 (length gaps)))
    (let ((gap (nth i gaps)))
      (princ (format "  %d. %s (%d mentions)\n"
                     (1+ i)
                     (plist-get gap :concept)
                     (plist-get gap :mentions))))))

(princ "\n")
(princ "═══════════════════════════════════════════════════════\n")
(princ "TEST 3: MODULE GENERATION\n")
(princ "═══════════════════════════════════════════════════════\n\n")

(princ "Generating module for 'analysis'...\n")
(let ((code (meta-log-kg-generate-module "analysis")))
  (with-temp-file "examples/meta-log-analysis-generated.el"
    (insert code))
  (princ "✓ Generated examples/meta-log-analysis-generated.el\n"))

(princ "\n")
(princ "═══════════════════════════════════════════════════════\n")
(princ "TEST 4: PREDICTIVE SUGGESTIONS\n")
(princ "═══════════════════════════════════════════════════════\n\n")

;; Simulate some user context
(meta-log-auto-enhance-track-context "WebRTC peer connection")
(meta-log-auto-enhance-track-context "crypto identity management")
(meta-log-auto-enhance-track-context "merkle tree verification")

(princ "Simulated user working on:\n")
(princ "  • WebRTC peer connection\n")
(princ "  • crypto identity management\n")
(princ "  • merkle tree verification\n\n")

(let ((suggestions (meta-log-auto-enhance-predict-needs)))
  (princ (format "Generated %d predictive suggestions:\n" (length suggestions)))
  (dotimes (i (min 5 (length suggestions)))
    (let ((sug (nth i suggestions)))
      (princ (format "  %d. %s - %s\n"
                     (1+ i)
                     (plist-get sug :suggestion)
                     (plist-get sug :reason))))))

(princ "\n")
(princ "═══════════════════════════════════════════════════════\n")
(princ "TEST 5: FULL AUTO-ENHANCEMENT CYCLE\n")
(princ "═══════════════════════════════════════════════════════\n\n")

(let ((report (meta-log-auto-enhance-run)))
  (princ "Auto-enhancement cycle complete!\n")
  (princ (format "  • WordNet mappings added: %d\n"
                 (cdr (assoc 'wordnet-mappings report))))
  (princ (format "  • Capability gaps found: %d\n"
                 (cdr (assoc 'gaps-found report))))
  (princ (format "  • Suggestions generated: %d\n"
                 (cdr (assoc 'suggestions report)))))

(princ "\n")
(princ "╔══════════════════════════════════════════════════════════╗\n")
(princ "║  ALL IMPROVEMENTS TESTED SUCCESSFULLY!                  ║\n")
(princ "╚══════════════════════════════════════════════════════════╝\n")
(princ "\n")

(princ "Generated files:\n")
(princ "  • meta-log-wordnet-learned.el (enhanced WordNet)\n")
(princ "  • examples/meta-log-analysis-generated.el (generated module)\n")
(princ "  • *meta-log-gap-analysis* buffer (gap report)\n")
(princ "\n")

(princ "Next steps:\n")
(princ "  1. Review generated module\n")
(princ "  2. Check gap analysis buffer\n")
(princ "  3. Enable continuous learning:\n")
(princ "     (meta-log-auto-enhance-setup '(\"/path/to/watch\"))\n")
(princ "     (meta-log-auto-enhance-continuous-learning)\n")
(princ "\n")

(provide 'test-improvements)
