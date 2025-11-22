;;; meta-log-automata.el --- automaton-evolutions loader

;; Copyright (C) 2025 Automaton System
;; Author: Automaton System
;; Version: 1.0.0

;; This file is part of meta-log.

;;; Commentary:

;; Load automata from automaton-evolutions npm package.

;;; Code:

(require 'cl-lib)
(require 'meta-log-geometric-consensus)

(defun meta-log-find-evolutions-package ()
  "Find automaton-evolutions package installation.
Returns path to package directory or nil."
  (or (getenv "META_LOG_AUTOMATA_PATH")
      (let ((npm-output (shell-command-to-string
                         "npm list -g automaton-evolutions 2>/dev/null | head -1")))
        (when (string-match "/\\(.*\\)" npm-output)
          (expand-file-name (match-string 1 npm-output))))
      (let ((local-path (expand-file-name "node_modules/automaton-evolutions" default-directory)))
        (when (file-exists-p local-path)
          local-path))))

(defun meta-log-list-automata-files (package-path)
  "List all automaton files in package.
PACKAGE-PATH is the path to automaton-evolutions package.
Returns list of file paths."
  (let ((files '())
        (files-path (expand-file-name "files" package-path)))
    (when (file-exists-p files-path)
      (dolist (file (directory-files files-path t "\\.canvasl$"))
        (push file files)))
    (nreverse files)))

(defun meta-log-load-automaton-file (file-path)
  "Load a CanvasL automaton file.
FILE-PATH is the path to the .canvasl file.
Extracts facts and loads into Prolog/Datalog databases."
  (when (file-exists-p file-path)
    (let ((buffer (find-file-noselect file-path)))
      (with-current-buffer buffer
        (let ((content (buffer-string)))
          (meta-log-parse-canvasl content))))))

(defun meta-log-parse-canvasl (content)
  "Parse CanvasL content and extract facts.
CONTENT is the CanvasL file content."
  (require 'meta-log-prolog)
  (require 'meta-log-datalog)
  (let ((lines (split-string content "\n"))
        (facts '()))
    (dolist (line lines)
      (when (and (> (length line) 0)
                 (not (string-prefix-p "@" line))
                 (not (string-prefix-p "#" line)))
        (let ((obj (condition-case nil
                       (json-read-from-string line)
                     (error nil))))
          (when obj
            (let ((facts-from-obj (meta-log-extract-facts-from-jsonl obj)))
              (setq facts (append facts facts-from-obj)))))))
    (dolist (fact facts)
      (apply 'meta-log-prolog-add-fact fact)
      (apply 'meta-log-datalog-add-fact fact))
    facts))

(defun meta-log-extract-facts-from-jsonl (obj)
  "Extract facts from a JSONL object.
OBJ is a parsed JSON object.
Returns list of facts."
  (let ((facts '())
        (id (cdr (assq 'id obj)))
        (type (cdr (assq 'type obj))))
    (when id
      (push (list 'node id type) facts)
      (let ((dimension (cdr (assq 'dimension obj))))
        (when dimension
          (push (list 'dimension id dimension) facts)))
      (let ((from-node (cdr (assq 'fromNode obj)))
            (to-node (cdr (assq 'toNode obj))))
        (when (and from-node to-node)
          (push (list 'edge id from-node to-node) facts))))
    facts))

(defun meta-log-load-all-automata (package-path)
  "Load all automata from automaton-evolutions package.
PACKAGE-PATH is the path to the package directory."
  (interactive "DPath to automaton-evolutions: ")
  (let ((files (meta-log-list-automata-files package-path)))
    (if files
        (progn
          (message "Loading %d automaton file(s)..." (length files))
          (dolist (file files)
            (meta-log-load-automaton-file file))
          (message "Loaded %d automaton file(s)" (length files)))
      (message "No automaton files found in %s" package-path))))

(defun meta-log-automata-classify-geometric-type (obj)
  "Classify automaton object by geometric type.
OBJ is a parsed JSONL object from CanvasL.
Returns geometric type symbol or nil."
  (let ((dimension (cdr (assq 'dimension obj)))
        (bipartite (cdr (assq 'bipartite obj))))
    (when dimension
      (cond
       ((string-match "0D" dimension) 'point)
       ((string-match "1D" dimension) 'line)
       ((string-match "2D" dimension) 'triangle)
       ((string-match "3D" dimension)
        (let ((vertices (cdr (assq 'vertices obj)))
              (type-str (cdr (assq 'type obj))))
          (cond
           ((or (eq vertices 4) (string-match "tetrahedron" (or type-str ""))) 'tetrahedron)
           ((or (eq vertices 8) (string-match "cube" (or type-str ""))) 'cube)
           ((or (eq vertices 6) (string-match "octahedron" (or type-str ""))) 'octahedron)
           ((or (eq vertices 20) (string-match "dodecahedron" (or type-str ""))) 'dodecahedron)
           ((or (eq vertices 12) (string-match "icosahedron" (or type-str ""))) 'icosahedron)
           (t 'tetrahedron))))
       ((string-match "4D" dimension)
        (let ((vertices (cdr (assq 'vertices obj))))
          (cond
           ((eq vertices 5) 'five-cell)
           ((eq vertices 16) 'eight-cell)
           ((eq vertices 8) 'sixteen-cell)
           ((eq vertices 24) 'twenty-four-cell)
           ((eq vertices 120) 'six-hundred-cell)
           (t 'five-cell))))
       (t nil)))))

(defun meta-log-automata-extract-geometric-consensus (obj)
  "Extract geometric consensus requirements from automaton object.
OBJ is a parsed JSONL object.
Returns list of consensus criteria (boolean values)."
  (let ((bipartite (cdr (assq 'bipartite obj)))
        (criteria '()))
    (when bipartite
      (let ((partition (cdr (assq 'partition bipartite)))
            (bqf (cdr (assq 'bqf bipartite))))
        (when (and partition bqf)
          (let ((coefficients (cdr (assq 'coefficients bqf))))
            (when coefficients
              (setq criteria (mapcar (lambda (coeff) (> coeff 0)) coefficients)))))))
    criteria))

(provide 'meta-log-automata)

;;; meta-log-automata.el ends here

