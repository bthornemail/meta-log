;;; coverage-config.el --- Coverage configuration for Emacs Lisp tests
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Configuration for code coverage analysis using undercover.el

;;; Code:

;; Check if undercover is available
(condition-case nil
    (progn
      (require 'undercover)
      (message "undercover.el loaded for coverage analysis"))
  (error
   (message "Warning: undercover.el not available. Install with: M-x package-install RET undercover RET")))

;; Coverage configuration
(when (fboundp 'undercover)
  ;; Set coverage report directory
  (setq undercover-force-coverage t)
  (setq undercover-report-format 'text)
  (setq undercover-report-dir (expand-file-name "coverage" (file-name-directory (or load-file-name default-directory))))
  
  ;; Files to include in coverage
  (setq undercover-include-paths
        (list (expand-file-name "../modules" (file-name-directory (or load-file-name default-directory)))
              (expand-file-name "../" (file-name-directory (or load-file-name default-directory)))))
  
  ;; Files to exclude from coverage
  (setq undercover-exclude-paths
        (list (expand-file-name "../tests" (file-name-directory (or load-file-name default-directory)))
              (expand-file-name "../dev-docs" (file-name-directory (or load-file-name default-directory)))
              (expand-file-name "../docs" (file-name-directory (or load-file-name default-directory)))))
  
  (message "Coverage configuration loaded"))

(provide 'coverage-config)

;;; coverage-config.el ends here

