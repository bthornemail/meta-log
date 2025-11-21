;;; meta-log-unix-types.el --- Unix file type topology mapping

;; Copyright (C) 2025 Automaton System
;; Author: Brian Thorne
;; Version: 1.0.0

;;; Commentary:

;; Maps Unix file types to meta-log's 0D-7D dimensional topology:
;; - Regular files, directories, symlinks, devices, pipes, sockets
;; - Integrates with inode system
;; - Maps to CanvasL automata
;; - Canvas API connections

;;; Code:

(require 'cl-lib)
(require 'meta-log-inode)

;;; Unix File Type Enumeration

(defconst meta-log-unix-file-types
  '((regular        . ?-)  ; Regular file
    (directory      . ?d)  ; Directory
    (symlink        . ?l)  ; Symbolic link
    (block-device   . ?b)  ; Block special file
    (char-device    . ?c)  ; Character special file
    (fifo           . ?p)  ; Named pipe (FIFO)
    (socket         . ?s)) ; Socket
  "Unix file types with their ls -l type characters.")

;;; Dimensional Topology Mapping

(defconst meta-log-unix-type-topology
  '(;; 0D: Identity - Regular files (static data)
    (regular . (:dimension "0D"
                :topology "point"
                :description "Regular file: static data at a point"
                :church-encoding "λx.x"
                :canvas-api "File"
                :automata-state "data-node"))

    ;; 1D: Temporal - FIFOs (sequential flow)
    (fifo . (:dimension "1D"
             :topology "line"
             :description "FIFO: temporal stream, sequential data flow"
             :church-encoding "λf.λx.f(x)"
             :canvas-api "Stream"
             :automata-state "stream-node"))

    ;; 2D: Structure - Directories (graph structure)
    (directory . (:dimension "2D"
                  :topology "graph"
                  :description "Directory: hierarchical graph structure"
                  :church-encoding "λx.λy.λf.fxy"
                  :canvas-api "FileSystem"
                  :automata-state "tree-node"))

    ;; 3D: Network - Symlinks (algebraic links)
    (symlink . (:dimension "3D"
              :topology "manifold"
              :description "Symlink: algebraic pointer, creates topology"
              :church-encoding "λf.λx.f(f x)"
              :canvas-api "Reference"
              :automata-state "link-node"))

    ;; 4D: Network - Sockets (network topology)
    (socket . (:dimension "4D"
               :topology "network"
               :description "Socket: network communication endpoint"
               :church-encoding "λf.λg.λx.f(g x)"
               :canvas-api "WebSocket"
               :automata-state "socket-node"))

    ;; 5D: Consensus - Block devices (shared state)
    (block-device . (:dimension "5D"
                     :topology "bundle"
                     :description "Block device: shared storage medium"
                     :church-encoding "λf.Y f"
                     :canvas-api "Storage"
                     :automata-state "storage-node"))

    ;; 6D: Intelligence - Character devices (interactive I/O)
    (char-device . (:dimension "6D"
                    :topology "feedback"
                    :description "Character device: interactive terminal I/O"
                    :church-encoding "λf.λx.f(λy.x x y)"
                    :canvas-api "Console"
                    :automata-state "terminal-node")))
  "Mapping of Unix file types to dimensional topology.")

;;; File Type Detection

(defun meta-log-unix-get-file-type (file-path)
  "Get Unix file type for FILE-PATH.
Returns symbol: regular, directory, symlink, etc."
  (when (file-exists-p file-path)
    (let* ((attrs (file-attributes file-path))
           (type (car attrs)))
      (cond
       ((eq type t) 'directory)
       ((stringp type) 'symlink)
       ((null type) 'regular)
       (t
        ;; Use stat for special files
        (let ((stat-output (shell-command-to-string
                           (format "stat -c '%%F' %s 2>/dev/null"
                                   (shell-quote-argument file-path)))))
          (cond
           ((string-match-p "block special file" stat-output) 'block-device)
           ((string-match-p "character special file" stat-output) 'char-device)
           ((string-match-p "fifo" stat-output) 'fifo)
           ((string-match-p "socket" stat-output) 'socket)
           (t 'regular))))))))

(defun meta-log-unix-get-topology (file-path)
  "Get topological properties for FILE-PATH.
Returns plist with dimension, topology type, Canvas API, etc."
  (let* ((file-type (meta-log-unix-get-file-type file-path))
         (topology (cdr (assoc file-type meta-log-unix-type-topology))))
    (when topology
      (append (list :file-type file-type :path file-path)
              topology))))

;;; Enhanced Inode Registration with Type Topology

(defun meta-log-unix-register-with-topology (file-path)
  "Register FILE-PATH with both inode and Unix type topology.
Returns enhanced inode-ref with topology metadata."
  (let* ((inode-ref (meta-log-inode-register-file file-path))
         (topology (meta-log-unix-get-topology file-path)))
    (when (and inode-ref topology)
      ;; Enhance inode-ref with topology
      (setf (meta-log-inode-ref-metadata inode-ref)
            (plist-put (meta-log-inode-ref-metadata inode-ref)
                      :unix-type (plist-get topology :file-type)))
      (setf (meta-log-inode-ref-metadata inode-ref)
            (plist-put (meta-log-inode-ref-metadata inode-ref)
                      :dimension (plist-get topology :dimension)))
      (setf (meta-log-inode-ref-metadata inode-ref)
            (plist-put (meta-log-inode-ref-metadata inode-ref)
                      :topology (plist-get topology :topology)))
      (setf (meta-log-inode-ref-metadata inode-ref)
            (plist-put (meta-log-inode-ref-metadata inode-ref)
                      :canvas-api (plist-get topology :canvas-api)))
      (setf (meta-log-inode-ref-metadata inode-ref)
            (plist-put (meta-log-inode-ref-metadata inode-ref)
                      :automata-state (plist-get topology :automata-state))))
    inode-ref))

;;; CanvasL Integration

(defun meta-log-unix-to-canvasl (file-path)
  "Convert FILE-PATH to CanvasL automata representation.
Returns CanvasL node JSON."
  (let* ((topology (meta-log-unix-get-topology file-path))
         (inode-info (meta-log-inode-get-info file-path)))
    (when topology
      (format "{\"id\":\"%s\",\"type\":\"%s\",\"state\":\"%s\",\"dimension\":\"%s\",\"path\":\"%s\",\"inode\":%d,\"device\":%d}"
              (format "file-%d-%d"
                     (plist-get inode-info :inode)
                     (plist-get inode-info :device))
              (plist-get topology :automata-state)
              (plist-get topology :file-type)
              (plist-get topology :dimension)
              file-path
              (plist-get inode-info :inode)
              (plist-get inode-info :device)))))

;;; Canvas API Mapping

(defconst meta-log-unix-canvas-api-map
  '((File . ((:method . "read")
             (:method . "write")
             (:event . "change")))

    (Stream . ((:method . "read")
               (:method . "write")
               (:event . "data")
               (:event . "end")))

    (FileSystem . ((:method . "readdir")
                   (:method . "mkdir")
                   (:method . "stat")
                   (:event . "change")))

    (Reference . ((:method . "resolve")
                  (:method . "target")
                  (:event . "broken")))

    (WebSocket . ((:method . "send")
                  (:method . "receive")
                  (:event . "open")
                  (:event . "message")
                  (:event . "close")))

    (Storage . ((:method . "read")
                (:method . "write")
                (:event . "mount")
                (:event . "unmount")))

    (Console . ((:method . "read")
                (:method . "write")
                (:event . "input")
                (:event . "resize"))))
  "Mapping of Canvas API types to methods and events.")

(defun meta-log-unix-get-canvas-api (file-path)
  "Get Canvas API for FILE-PATH.
Returns plist with API type, methods, and events."
  (let* ((topology (meta-log-unix-get-topology file-path))
         (canvas-api-str (plist-get topology :canvas-api)))
    (if canvas-api-str
        (let* ((api-type (intern canvas-api-str))
               (api-spec (cdr (assoc api-type meta-log-unix-canvas-api-map))))
          (when api-spec
            (list :type api-type
                  :methods (mapcar #'cdr (cl-remove-if-not
                                         (lambda (x) (eq (car x) :method))
                                         api-spec))
                  :events (mapcar #'cdr (cl-remove-if-not
                                        (lambda (x) (eq (car x) :event))
                                        api-spec)))))
      ;; No canvas-api, return empty
      (list :type nil :methods nil :events nil))))

;;; Knowledge Graph Integration

(defun meta-log-unix-kg-register (file-path)
  "Register FILE-PATH in knowledge graph with Unix type topology.
Returns KG node with full topology metadata."
  (require 'meta-log-knowledge-graph)

  (let* ((inode-ref (meta-log-unix-register-with-topology file-path))
         (topology (meta-log-unix-get-topology file-path))
         (canvas-api (meta-log-unix-get-canvas-api file-path)))

    ;; Create KG node
    (let ((node (make-meta-log-kg-node
                 :id (format "unix-%d-%d"
                           (meta-log-inode-ref-inode inode-ref)
                           (meta-log-inode-ref-device inode-ref))
                 :type 'unix-file
                 :label (file-name-nondirectory file-path)
                 :dimension (plist-get topology :dimension)
                 :semantic-field (plist-get topology :topology)
                 :source-file file-path
                 :properties (list
                             :file-type (plist-get topology :file-type)
                             :inode (meta-log-inode-ref-inode inode-ref)
                             :device (meta-log-inode-ref-device inode-ref)
                             :topology (plist-get topology :topology)
                             :church-encoding (plist-get topology :church-encoding)
                             :canvas-api (plist-get canvas-api :type)
                             :api-methods (plist-get canvas-api :methods)
                             :api-events (plist-get canvas-api :events)))))

      ;; Add to KG
      (puthash (meta-log-kg-node-id node) node meta-log-kg--document-index)

      ;; Add to Prolog if available
      (when (fboundp 'meta-log-prolog-assert)
        (meta-log-prolog-assert
         (format "unix_file('%s', '%s', '%s', %d, %d)"
                (meta-log-kg-node-id node)
                (plist-get topology :file-type)
                (plist-get topology :dimension)
                (meta-log-inode-ref-inode inode-ref)
                (meta-log-inode-ref-device inode-ref))))

      node)))

;;; Directory Tree Mapping

(defun meta-log-unix-map-tree (root-path &optional max-depth)
  "Map entire directory tree at ROOT-PATH to topology.
MAX-DEPTH limits recursion depth (default: unlimited).
Returns list of topology nodes."
  (let ((nodes '())
        (depth 0))
    (cl-labels ((walk-tree
                 (path current-depth)
                 (when (or (not max-depth)
                          (<= current-depth max-depth))
                   (let ((topo (meta-log-unix-get-topology path)))
                     (when topo
                       (push topo nodes))

                     ;; Recurse into directories
                     (when (eq (plist-get topo :file-type) 'directory)
                       (dolist (file (directory-files path t "^[^.]"))
                         (walk-tree file (1+ current-depth))))))))

      (walk-tree root-path 0))
    (nreverse nodes)))

;;; Statistics and Reporting

(defun meta-log-unix-topology-stats (root-path)
  "Generate topology statistics for ROOT-PATH.
Shows distribution of file types by dimension."
  (interactive "DRoot directory: ")

  (let ((nodes (meta-log-unix-map-tree root-path 5))
        (dimension-counts (make-hash-table :test 'equal))
        (type-counts (make-hash-table :test 'equal)))

    ;; Count by dimension and type
    (dolist (node nodes)
      (let ((dim (plist-get node :dimension))
            (type (plist-get node :file-type)))
        (puthash dim (1+ (or (gethash dim dimension-counts) 0))
                dimension-counts)
        (puthash type (1+ (or (gethash type type-counts) 0))
                type-counts)))

    ;; Report
    (with-output-to-temp-buffer "*Unix Topology Stats*"
      (princ "╔══════════════════════════════════════════════════════════╗\n")
      (princ "║  UNIX FILE TOPOLOGY STATISTICS                          ║\n")
      (princ "╚══════════════════════════════════════════════════════════╝\n\n")

      (princ (format "Root: %s\n" root-path))
      (princ (format "Total files: %d\n\n" (length nodes)))

      (princ "Distribution by Dimension:\n")
      (princ "==========================\n")
      (maphash (lambda (dim count)
                 (princ (format "  %s: %4d files\n" dim count)))
              dimension-counts)

      (princ "\nDistribution by Type:\n")
      (princ "=====================\n")
      (maphash (lambda (type count)
                 (princ (format "  %-15s: %4d files\n" type count)))
              type-counts)

      (princ "\nTopology Mapping:\n")
      (princ "=================\n")
      (dolist (mapping meta-log-unix-type-topology)
        (let ((type (car mapping))
              (props (cdr mapping)))
          (princ (format "  %s → %s (%s)\n"
                        type
                        (plist-get props 'dimension)
                        (plist-get props 'topology))))))))

(provide 'meta-log-unix-types)

;;; meta-log-unix-types.el ends here
