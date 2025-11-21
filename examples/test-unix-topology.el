;;; test-unix-topology.el --- Test Unix file topology mapping

(add-to-list 'load-path "/data/data/com.termux/files/home/github/meta-log")
(require 'meta-log-unix-types)

(princ "\n")
(princ "╔══════════════════════════════════════════════════════════╗\n")
(princ "║  UNIX FILE TYPE TOPOLOGY MAPPING TEST                  ║\n")
(princ "╚══════════════════════════════════════════════════════════╝\n")
(princ "\n")

;; Test 1: Map individual file types
(princ "TEST 1: Individual File Topology\n")
(princ "=================================\n\n")

(let ((test-files '("/data/data/com.termux/files/home/github/meta-log/meta-log.el"
                   "/data/data/com.termux/files/home/github/meta-log/examples"
                   "/data/data/com.termux/files/home/automaton/grok_files/01-Grok.md")))

  (dolist (file test-files)
    (when (file-exists-p file)
      (let* ((ftype (meta-log-unix-get-file-type file))
             (topo (meta-log-unix-get-topology file))
             (canvas-api (meta-log-unix-get-canvas-api file)))

        (princ (format "File: %s\n" (file-name-nondirectory file)))
        (princ (format "  Type: %s\n" ftype))
        (princ (format "  Dimension: %s\n" (plist-get topo :dimension)))
        (princ (format "  Topology: %s\n" (plist-get topo :topology)))
        (princ (format "  Church: %s\n" (plist-get topo :church-encoding)))
        (princ (format "  Canvas API: %s\n" (plist-get canvas-api :type)))
        (princ (format "  Methods: %s\n" (plist-get canvas-api :methods)))
        (princ "\n")))))

(princ "TEST 2: Unix Type to Dimension Mapping\n")
(princ "=======================================\n\n")

(dolist (mapping meta-log-unix-type-topology)
  (let ((type (car mapping))
        (props (cdr mapping)))
    (princ (format "%-15s → %s (%s)\n"
                  type
                  (plist-get props 'dimension)
                  (plist-get props 'topology)))))

(princ "\n")
(princ "TEST 3: CanvasL JSON Generation\n")
(princ "================================\n\n")

(let ((test-file "/data/data/com.termux/files/home/automaton/grok_files/01-Grok.md"))
  (when (file-exists-p test-file)
    (let ((canvasl (meta-log-unix-to-canvasl test-file)))
      (princ "CanvasL JSON:\n")
      (princ canvasl)
      (princ "\n\n"))))

(princ "TEST 4: Directory Tree Mapping\n")
(princ "===============================\n\n")

(let ((nodes (meta-log-unix-map-tree "/data/data/com.termux/files/home/automaton/grok_files" 0)))
  (princ (format "Found %d files\n" (length nodes)))
  (let ((dim-dist (make-hash-table :test 'equal)))
    (dolist (node nodes)
      (let ((dim (plist-get node :dimension)))
        (puthash dim (1+ (or (gethash dim dim-dist) 0)) dim-dist)))

    (princ "\nDimension Distribution:\n")
    (maphash (lambda (dim count)
               (princ (format "  %s: %d files\n" (or dim "nil") count)))
            dim-dist)))

(princ "\n")
(princ "✓ Unix Topology Mapping Tests Complete\n")

(provide 'test-unix-topology)
