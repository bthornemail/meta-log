;;; test-inode-addressing.el --- Demonstrate inode-based file addressing

(add-to-list 'load-path "/data/data/com.termux/files/home/github/meta-log")
(require 'meta-log-inode)

(princ "\n")
(princ "╔══════════════════════════════════════════════════════════╗\n")
(princ "║  INODE-BASED FILE ADDRESSING DEMONSTRATION              ║\n")
(princ "╚══════════════════════════════════════════════════════════╝\n")
(princ "\n")

;; Test 1: Register a file and get its inode
(princ "TEST 1: File Registration\n")
(princ "=========================\n")

(let* ((test-file "/data/data/com.termux/files/home/github/meta-log/meta-log.el")
       (ref (meta-log-inode-register-file test-file)))

  (princ (format "File: %s\n" test-file))
  (princ (format "Inode: %d\n" (meta-log-inode-ref-inode ref)))
  (princ (format "Device: %d\n" (meta-log-inode-ref-device ref)))
  (princ (format "Paths: %s\n\n" (meta-log-inode-ref-paths ref))))

;; Test 2: Create a hard link and show it's detected
(princ "TEST 2: Hard Link Detection\n")
(princ "===========================\n")

(let ((original "/data/data/com.termux/files/home/github/meta-log/examples/test-original.txt")
      (hardlink "/data/data/com.termux/files/home/github/meta-log/examples/test-hardlink.txt"))

  ;; Create test file
  (with-temp-file original
    (insert "This is test content for hard link detection."))

  ;; Create hard link
  (shell-command (format "ln %s %s" original hardlink))

  ;; Register both
  (let ((ref1 (meta-log-inode-register-file original))
        (ref2 (meta-log-inode-register-file hardlink)))

    (princ (format "Original file: %s (inode: %d)\n"
                   original (meta-log-inode-ref-inode ref1)))
    (princ (format "Hard link: %s (inode: %d)\n"
                   hardlink (meta-log-inode-ref-inode ref2)))
    (princ (format "Same inode? %s\n"
                   (if (= (meta-log-inode-ref-inode ref1)
                         (meta-log-inode-ref-inode ref2))
                       "YES ✓" "NO")))
    (princ (format "All paths to this inode: %s\n\n"
                   (meta-log-inode-ref-paths ref1))))

  ;; Cleanup
  (delete-file original)
  (delete-file hardlink))

;; Test 3: Demonstrate rename resilience
(princ "TEST 3: Rename Resilience\n")
(princ "=========================\n")

(let ((original-path "/data/data/com.termux/files/home/github/meta-log/examples/test-original-name.txt")
      (renamed-path "/data/data/com.termux/files/home/github/meta-log/examples/test-renamed.txt"))

  ;; Create and register file
  (with-temp-file original-path
    (insert "Content that survives rename!"))

  (let* ((ref (meta-log-inode-register-file original-path))
         (inode (meta-log-inode-ref-inode ref))
         (device (meta-log-inode-ref-device ref)))

    (princ (format "Created: %s (inode: %d)\n" original-path inode))

    ;; Rename the file
    (rename-file original-path renamed-path t)
    (princ (format "Renamed to: %s\n" renamed-path))

    ;; Can we still access it by inode?
    (let ((resolved-path (meta-log-inode-resolve (cons device inode))))
      (princ (format "Resolved by inode: %s\n" resolved-path))
      (princ (format "Resolution successful? %s\n"
                     (if resolved-path "YES ✓" "NO")))

      ;; Read content via inode
      (let ((content (meta-log-inode-read-file (cons device inode))))
        (princ (format "Content via inode: \"%s\"\n\n"
                       (string-trim content)))))

    ;; Cleanup
    (when (file-exists-p renamed-path)
      (delete-file renamed-path))))

;; Test 4: Deduplication detection
(princ "TEST 4: Duplicate Detection\n")
(princ "===========================\n")

(let ((file1 "/data/data/com.termux/files/home/github/meta-log/examples/test-dup1.txt")
      (file2 "/data/data/com.termux/files/home/github/meta-log/examples/test-dup2.txt")
      (file3 "/data/data/com.termux/files/home/github/meta-log/examples/test-unique.txt"))

  ;; Create duplicate files
  (with-temp-file file1
    (insert "Identical content in both files"))
  (with-temp-file file2
    (insert "Identical content in both files"))
  (with-temp-file file3
    (insert "Different content here"))

  ;; Register all
  (meta-log-inode-register-file file1)
  (meta-log-inode-register-file file2)
  (meta-log-inode-register-file file3)

  ;; Find duplicates
  (let ((duplicates (meta-log-inode-find-duplicates)))
    (princ (format "Found %d sets of duplicate content\n" (length duplicates)))
    (dolist (dup duplicates)
      (princ (format "  Hash: %s\n" (substring (car dup) 0 16)))
      (princ (format "  Files: %s\n" (cdr dup)))))

  ;; Cleanup
  (delete-file file1)
  (delete-file file2)
  (delete-file file3))

(princ "\n")

;; Test 5: Registry statistics
(princ "TEST 5: Registry Statistics\n")
(princ "===========================\n")
(meta-log-inode-registry-stats)

(princ "\n")
(princ "╔══════════════════════════════════════════════════════════╗\n")
(princ "║  ADVANTAGES OF INODE-BASED ADDRESSING                  ║\n")
(princ "╚══════════════════════════════════════════════════════════╝\n")
(princ "\n")
(princ "✓ Files remain accessible even after rename/move\n")
(princ "✓ Hard links automatically detected\n")
(princ "✓ Duplicate content discovered\n")
(princ "✓ Content-addressable storage semantics\n")
(princ "✓ Knowledge graph links survive file reorganization\n")
(princ "✓ Topologically invariant references\n")
(princ "\n")

(provide 'test-inode-addressing)
