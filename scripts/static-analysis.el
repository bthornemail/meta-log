;;; static-analysis.el --- Static analysis for unused code
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Script to identify unused functions, imports, and dead code.

;;; Code:

(require 'cl-lib)

(defvar static-analysis--function-defs '()
  "List of all function definitions found.")

(defvar static-analysis--function-calls '()
  "List of all function calls found.")

(defvar static-analysis--unused-functions '()
  "List of functions that appear to be unused.")

(defun static-analysis--find-defuns (directory)
  "Find all defun definitions in DIRECTORY."
  (let ((files (directory-files-recursively directory "\\.el$"))
        (defuns '()))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(defun \\([^ ]+\\)" nil t)
          (let ((func-name (match-string 1))
                (file-path (file-relative-name file directory)))
                (line-num (line-number-at-pos)))
            (push (list func-name file-path line-num) defuns)))))
    defuns))

(defun static-analysis--find-function-calls (directory)
  "Find all function calls in DIRECTORY."
  (let ((files (directory-files-recursively directory "\\.el$"))
        (calls '()))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Match function calls: (function-name ...)
        (while (re-search-forward "(\\([a-zA-Z0-9-]+\\)[^a-zA-Z0-9-]" nil t)
          (let ((func-name (match-string 1))
                (file-path (file-relative-name file directory)))
            (when (and (not (string-prefix-p "meta-log-" func-name))
                       (string-match-p "^meta-log-" func-name))
              (push func-name calls)))))
    (delete-dups calls)))

(defun static-analysis--find-scheme-defines (directory)
  "Find all define definitions in Scheme files."
  (let ((files (directory-files-recursively directory "\\.scm$"))
        (defines '()))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(define (\\([^ ]+\\)" nil t)
          (let ((func-name (match-string 1))
                (file-path (file-relative-name file directory))
                (line-num (line-number-at-pos)))
            (push (list func-name file-path line-num) defines)))))
    defines))

(defun static-analysis--check-function-usage (func-name directory)
  "Check if FUNC-NAME is called anywhere in DIRECTORY."
  (let ((files (directory-files-recursively directory "\\.\\(el\\|scm\\)$"))
        (found nil))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (search-forward func-name nil t)
          (setq found t)
          (cl-return))))
    found))

(defun static-analysis--analyze-modules ()
  "Analyze meta-log modules for unused code."
  (let ((modules-dir (expand-file-name "modules" default-directory))
        (scheme-dir (expand-file-name "scheme" default-directory))
        (results '()))
    
    (message "Analyzing Emacs Lisp modules...")
    (let ((defuns (static-analysis--find-defuns modules-dir)))
      (dolist (defun-info defuns)
        (let ((func-name (car defun-info))
              (file-path (cadr defun-info))
              (line-num (caddr defun-info)))
          (unless (or (string-match-p "--" func-name)  ; Internal/private functions
                      (string-match-p "test-" func-name)  ; Test functions
                      (static-analysis--check-function-usage func-name default-directory))
            (push (list func-name file-path line-num) results)))))
    
    (message "Analyzing Scheme modules...")
    (let ((defines (static-analysis--find-scheme-defines scheme-dir)))
      (dolist (define-info defines)
        (let ((func-name (car define-info))
              (file-path (cadr define-info))
              (line-num (caddr define-info)))
          (unless (or (string-match-p "test-" func-name)  ; Test functions
                      (string-match-p "helper" func-name)  ; Helper functions
                      (static-analysis--check-function-usage func-name default-directory))
            (push (list func-name file-path line-num) results)))))
    
    results))

(defun static-analysis-run ()
  "Run static analysis and generate report."
  (interactive)
  (let ((results (static-analysis--analyze-modules))
        (report-file (expand-file-name "docs/STATIC-ANALYSIS-REPORT.md" default-directory)))
    (with-temp-file report-file
      (insert "# Static Analysis Report\n\n")
      (insert "**Date**: " (format-time-string "%Y-%m-%d") "\n\n")
      (insert "## Unused Functions\n\n")
      (if results
          (progn
            (insert "Found " (number-to-string (length results)) " potentially unused functions:\n\n")
            (insert "| Function | File | Line | Status |\n")
            (insert "|----------|------|------|--------|\n")
            (dolist (result results)
              (let ((func-name (car result))
                    (file-path (cadr result))
                    (line-num (caddr result)))
                (insert (format "| `%s` | `%s` | %d | ⚠️ Potentially unused |\n"
                               func-name file-path line-num)))))
        (insert "✅ No unused functions found.\n\n"))
      (insert "\n## Notes\n\n")
      (insert "- Functions marked as 'internal' (containing `--`) are excluded\n")
      (insert "- Test functions are excluded\n")
      (insert "- Manual review recommended before removal\n"))
    (message "Static analysis complete. Report saved to %s" report-file)))

(provide 'static-analysis)

;;; static-analysis.el ends here

