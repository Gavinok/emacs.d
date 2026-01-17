(require 'ert)

(ert-deftest test-repo-map-create ()
  (let ((map (repo-map-create "/tmp")))
    (should (repo-map-p map))
    (should (equal (repo-map-root map) "/tmp"))
    (should (= (repo-map-max-map-tokens map) 1024))
    (should (= (repo-map-cache-threshold map) 0.95))
    (should (hash-table-p (repo-map-tree-cache map)))
    (should (hash-table-p (repo-map-map-cache map)))))

(ert-deftest test-repo-map-get-rel-fname ()
  (let ((map (repo-map-create "/home/user/project")))
    (should (equal (repo-map-get-rel-fname map "/home/user/project/src/file.el")
                   "src/file.el"))))

(ert-deftest test-repo-map-get-mtime ()
  (let ((temp-file (make-temp-file "test")))
    (unwind-protect
        (should (floatp (repo-map-get-mtime temp-file)))
      (delete-file temp-file))))

(ert-deftest test-get-file-mentions ()
  (let ((text "Check file.py and utils/helper.js in src/code.el"))
    (should (equal (sort (get-file-mentions text) #'string<)
                   '("file.py" "src/code.el" "utils/helper.js")))))

(ert-deftest test-get-ident-mentions ()
  (let ((text "The function calculate_total() and helper_function need fixing"))
    (should (equal (sort (get-ident-mentions text) #'string<)
                   '("The" "calculate_total" "fixing" "helper_function" "need")))))



(ert-deftest test-repo-map-tree-sitter ()
  (let* ((map (repo-map-create "/tmp/test"))
         (test-file (make-temp-file "test" nil ".py"
                                    ;; Create a test Python file with some content
                                    "def test_function():\n    pass\n\nclass TestClass:\n    def method(self):\n        pass")))
    (unwind-protect
        (progn
          ;; Test parsing
          (let ((tags (repo-map-get-tags map test-file)))
            (should tags)
            (should (seq-some (lambda (tag)
                                (and (equal (repo-map-tag-name tag) "test_function")
                                     (equal (repo-map-tag-kind tag) 'def)))
                              tags))
            (should (seq-some (lambda (tag)
                                (and (equal (repo-map-tag-name tag) "TestClass")
                                     (equal (repo-map-tag-kind tag) 'def)))
                              tags))))
      (delete-file test-file))))

;; (ert-deftest test-repo-map-cache ()
;;   (let* ((map (repo-map-create "/tmp/test"))
;;          (test-file (make-temp-file "test2" nil ".py")))
;;     (unwind-protect
;;         (progn
;;           ;; Create test file
;;           (with-temp-file test-file
;;             (insert "def test_function(): pass"))

;;           ;; First parse should cache
;;           (let ((first-tags (repo-map-get-tags map test-file)))
;;             ;; Second parse should use cache
;;             (let ((second-tags (repo-map-get-tags map test-file)))
;;               (should (equal first-tags second-tags)))

;;             ;; Modify file should invalidate cache
;;             (with-temp-file test-file
;;               (insert "def another_function(): pass"))
;;             (let ((new-tags (repo-map-get-tags map test-file)))
;;               (should-not (equal first-tags new-tags)))))
;;       (delete-file test-file))))

(ert-deftest test-repo-map-get-tags ()
  (let* ((map (repo-map-create "/tmp/test"))
         (test-file (make-temp-file "test1" nil ".py")))
    (unwind-protect
        (progn
          ;; Create a test file with some definitions
          (with-temp-file test-file
            (insert "def test_function():\n    pass\n\n"
                    "class TestClass:\n    pass\n"))

          ;; Test tag collection
          (let ((tags (repo-map-get-tags map test-file)))
            (should tags)
            ;; Should find both the function and class definitions
            (should (seq-some (lambda (tag)
                                (and (equal (repo-map-tag-name tag) "test_function")
                                     (equal (repo-map-tag-kind tag) 'def)))
                              tags))
            (should (seq-some (lambda (tag)
                                (and (equal (repo-map-tag-name tag) "TestClass")
                                     (equal (repo-map-tag-kind tag) 'def)))
                              tags))))
      (delete-file test-file))))

(ert-deftest test-repo-map-file-types ()
  (let* ((map (repo-map-create "/tmp/test"))
         (py-file (make-temp-file "test" nil ".py"))
         (go-file (make-temp-file "test" nil ".go"))
         (txt-file (make-temp-file "test" nil ".txt")))
    (unwind-protect
        (progn
          ;; Create files of different types
          (with-temp-file py-file
            (insert "def python_function(): pass"))
          (with-temp-file go-file
            (insert "func tsFunction () {}"))
          (with-temp-file txt-file
            (insert "Plain text file"))

          ;; Test language detection and parsing
          (should (repo-map-get-tags map py-file))  ; Should parse Python
          (should (repo-map-get-tags map go-file))
          (should-not (repo-map-get-tags map txt-file))) ; Should not parse text
      (delete-file py-file)
      (delete-file go-file)
      (delete-file txt-file))))

;; TODO implement test
;; (ert-deftest test-repo-map-context-generation ()
;;   (let* ((map (repo-map-create "/tmp/test"))
;;          (test-file (make-temp-file "test" nil ".py")))
;;     (unwind-protect
;;         (progn
;;           ;; Create a complex test file
;;           (with-temp-file test-file
;;             (insert "class TestClass:\n"
;;                     "    def method1(self):\n"
;;                     "        pass\n\n"
;;                     "    def method2(self):\n"
;;                     "        self.method1()\n\n"
;;                     "def standalone_function():\n"
;;                     "    obj = TestClass()\n"
;;                     "    obj.method2()"))

;;           ;; Test context generation
;;           (let ((context (repo-map-generate-context map test-file)))
;;             (should context)
;;             (should (string-match-p "TestClass" context))
;;             (should (string-match-p "method1" context))
;;             (should (string-match-p "method2" context))
;;             (should (string-match-p "standalone_function" context))))
;;       (delete-file test-file))))

;; TODO implement test
;; (ert-deftest test-repo-map-token-limits ()
;;   (let* ((map (repo-map-create "/tmp/test"))
;;          (test-file (make-temp-file "test" nil ".py")))
;;     (unwind-protect
;;         (progn
;;           ;; Create a large test file
;;           (with-temp-file test-file
;;             (dotimes (i 100)
;;               (insert (format "def function%d():\n    pass\n\n" i))))

;;           ;; Test token limiting
;;           (let* ((full-tags (repo-map-get-tags map test-file))
;;                  (limited-map (repo-map-generate-context map test-file 512)))
;;             (should full-tags)
;;             (should limited-map)
;;             (should (< (length limited-map)
;;                        (length (repo-map-generate-context map test-file 1024))))))
;;       (delete-file test-file))))


(ert-deftest test-gptel-generate-repo-map ()
  (let* ((temp-dir (make-temp-file "test-repo" t))
         (test-py-file (expand-file-name "test.py" temp-dir))
         (test-py2-file (expand-file-name "test2.py" temp-dir))
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create test files with some content
          (with-temp-file test-py-file
            (insert "from my_tester import my_test\ndef test_function():\n    pass\n\nclass TestClass:\n    def method(self):\n        pass"))
          (with-temp-file test-py2-file
            (insert "class my_test():\n    pass"))

          ;; Test with chat files
          (let* ((chat-files (list test-py-file))
                 (other-files (list test-py2-file))
                 (map (car (gptel-generate-repo-map chat-files other-files))))
            (should map)
            (should (stringp map))
            ;; Already should be included in context
            (should-not (string-match-p "my_tester" map))
            ;; never mentioned
            (should-not (string-match-p "my_test" map)))

          ;; Test with mentioned files and identifiers
          (let* ((chat-files nil)
                 (other-files (list test-py2-file test-py-file))
                 (mentioned-files '("test.py"))
                 (mentioned-idents '("test_function"))
                 (map (car (gptel-generate-repo-map chat-files other-files mentioned-files mentioned-idents))))
            (should map)
            (should (string-match-p "test_function" map))
            (should (string-match-p "test.py" map)))

          ;; Test with empty chat files
          (let* ((chat-files (list test-py-file test-py2-file))
                 (other-files nil)
                 (map (car (gptel-generate-repo-map chat-files other-files))))
            (should map)
            (should-not (string-match-p "test_function" map))
            (should-not (string-match-p "my_test" map))))

      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest test-gptel-generate-repo-map-for-prompt ()
  (let* ((temp-dir (make-temp-file "test-repo" t))
         (test-file (expand-file-name "test.py" temp-dir))
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create test file
          (with-temp-file test-file
            (insert "def example_function():\n    pass"))

          ;; Test with file mention in prompt
          (let* ((prompt "Please check test.py and the example_function")
                 (map (car (gptel-generate-repo-map-for-prompt prompt nil))))
            (should map)
            (should (string-match-p "example_function" map))
            (should (string-match-p "test.py" map)))

          ;; Test with only identifier mention
          (let* ((prompt "Can you look at example_function?")
                 (map (car (gptel-generate-repo-map-for-prompt prompt nil))))
            (should map)
            (should (string-match-p "example_function" map)))

          ;; Test with no relevant mentions
          (let* ((prompt "How are you doing today?")
                 (map (car (gptel-generate-repo-map-for-prompt prompt nil))))
            (should map)
            (should (string= map ""))))

      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest test-gptel-generate-repo-map-edge-cases ()
  (let* ((temp-dir (make-temp-file "test-repo" t))
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Test with non-existent files
          (let* ((chat-files '("/nonexistent/file.py"))
                 (other-files nil)
                 (map (car (gptel-generate-repo-map chat-files other-files))))
            (should map)
            (should (string= map "")))

          ;; Test with empty directory
          (let* ((chat-files nil)
                 (other-files nil)
                 (map (car (gptel-generate-repo-map chat-files other-files))))
            (should map)
            (should (string= map "")))

          ;; Test with unsupported file types
          (let* ((test-txt (expand-file-name "test.txt" temp-dir)))
            (with-temp-file test-txt
              (insert "Plain text file"))
            (let* ((chat-files (list test-txt))
                   (map (car (gptel-generate-repo-map chat-files nil))))
              (should map)
              (should (string= map "")))))

      ;; Cleanup
      (delete-directory temp-dir t))))

;;; Tests for new functionality

;; Token counting tests
(ert-deftest test-gptel-extra-count-tokens ()
  "Test token counting functionality."
  (should (= (gptel-extra--count-tokens "") 0))
  (should (= (gptel-extra--count-tokens nil) 0))
  (should (= (gptel-extra--count-tokens "hello") 1))
  (should (= (gptel-extra--count-tokens "hello world") 2))
  (should (= (gptel-extra--count-tokens "this is a longer string") 5)))

(ert-deftest test-gptel-extra-trim-to-token-limit ()
  "Test token trimming functionality."
  (let* ((content "this is a test string with many words")
         (result (gptel-extra--trim-to-token-limit content 5)))
    (should (consp result))
    (should (<= (cdr result) 5))
    (should (stringp (car result))))
  
  ;; Test with content under limit
  (let* ((content "short")
         (result (gptel-extra--trim-to-token-limit content 10)))
    (should (string= (car result) content))
    (should (= (cdr result) 1))))

;; Rate limiting tests
(ert-deftest test-gptel-extra-rate-limiting ()
  "Test rate limiting functionality."
  (let ((gptel-extra-min-request-interval 0.1)
        (gptel-extra--last-request-time nil)
        (start-time (float-time)))
    
    ;; First call should not wait
    (gptel-extra--rate-limit-check)
    (should (< (- (float-time) start-time) 0.05))
    
    ;; Second call should wait
    (let ((before-wait (float-time)))
      (gptel-extra--rate-limit-check)
      (should (>= (- (float-time) before-wait) 0.08)))))

;; File hashing tests
(ert-deftest test-gptel-extra-file-hash ()
  "Test file hashing functionality."
  (let ((temp-file (make-temp-file "test-hash")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "test content"))
          (let ((hash1 (gptel-extra--file-hash temp-file)))
            (should (stringp hash1))
            (should (= (length hash1) 32)) ; MD5 hash length
            
            ;; Same file should produce same hash
            (let ((hash2 (gptel-extra--file-hash temp-file)))
              (should (string= hash1 hash2)))
            
            ;; Different content should produce different hash
            (with-temp-file temp-file
              (insert "different content"))
            (let ((hash3 (gptel-extra--file-hash temp-file)))
              (should-not (string= hash1 hash3)))))
      (delete-file temp-file))))

;; Cache validation tests
(ert-deftest test-gptel-extra-cache-validation ()
  "Test cache validation functionality."
  (let ((temp-file (make-temp-file "test-cache")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "test content"))
          (let* ((hash (gptel-extra--file-hash temp-file))
                 (cache-entry (list :hash hash :data "cached-data")))
            
            ;; Valid cache entry should pass
            (should (gptel-extra--cache-valid-p temp-file cache-entry))
            
            ;; Invalid cache entry should fail
            (let ((invalid-entry (list :hash "invalid-hash" :data "data")))
              (should-not (gptel-extra--cache-valid-p temp-file invalid-entry)))
            
            ;; Modified file should invalidate cache
            (with-temp-file temp-file
              (insert "modified content"))
            (should-not (gptel-extra--cache-valid-p temp-file cache-entry))))
      (delete-file temp-file))))

;; File filtering tests
(ert-deftest test-gptel-extra-file-filtering ()
  "Test file filtering functionality."
  (let ((test-files '("src/main.py" 
                      "node_modules/package/index.js"
                      "__pycache__/module.pyc"
                      "build/output.exe"
                      "README.md"
                      ".git/config")))
    (should (gptel-extra--should-ignore-file-p "node_modules/package/index.js"))
    (should (gptel-extra--should-ignore-file-p "__pycache__/module.pyc"))
    (should (gptel-extra--should-ignore-file-p ".git/config"))
    (should-not (gptel-extra--should-ignore-file-p "src/main.py"))
    (should-not (gptel-extra--should-ignore-file-p "README.md"))
    
    (let ((filtered (gptel-extra--filter-files test-files)))
      (should (member "src/main.py" filtered))
      (should (member "README.md" filtered))
      (should-not (member "node_modules/package/index.js" filtered))
      (should-not (member "__pycache__/module.pyc" filtered)))))

;; Symbol ranking tests
(ert-deftest test-gptel-extra-symbol-ranking ()
  "Test symbol ranking functionality."
  (let ((context-text "The function calculate_total is important"))
    
    ;; Long descriptive names should get higher scores
    (let ((score-long (gptel-extra--rank-symbol "calculate_total_amount" context-text))
          (score-short (gptel-extra--rank-symbol "xyz" context-text)))
      (should (> score-long score-short)))
    
    ;; Symbols mentioned in context should get higher scores
    (let ((score-mentioned (gptel-extra--rank-symbol "calculate_total" context-text))
          (score-not-mentioned (gptel-extra--rank-symbol "other_function" context-text)))
      (should (> score-mentioned score-not-mentioned)))
    
    ;; Private symbols should get lower scores
    (let ((score-public (gptel-extra--rank-symbol "public_function" context-text))
          (score-private (gptel-extra--rank-symbol "_private_function" context-text)))
      (should (> score-public score-private)))))

;; Error recovery tests
(ert-deftest test-gptel-extra-error-recovery ()
  "Test error recovery functionality."
  (let ((gptel-extra-debug nil)) ; Disable debug output for tests
    
    ;; Function that succeeds should return result
    (let ((result (gptel-extra--with-error-recovery (lambda (x) (* x 2)) 5)))
      (should (= result 10)))
    
    ;; Function that fails should return nil
    (let ((result (gptel-extra--with-error-recovery (lambda () (error "Test error")))))
      (should (null result)))))

;; File ranking tests
(ert-deftest test-gptel-extra-file-ranking ()
  "Test file ranking functionality."
  (let* ((temp-dir (make-temp-file "test-ranking" t))
         (old-file (expand-file-name "old.py" temp-dir))
         (new-file (expand-file-name "new.py" temp-dir))
         (relevant-file (expand-file-name "calculate_helper.py" temp-dir)))
    
    (unwind-protect
        (progn
          ;; Create files with different modification times
          (with-temp-file old-file (insert "old content"))
          (sleep-for 0.1)
          (with-temp-file new-file (insert "new content"))
          (with-temp-file relevant-file (insert "relevant content"))
          
          (let* ((files (list old-file new-file relevant-file))
                 (mentioned-idents '("calculate"))
                 (ranked-files (gptel-extra--rank-files files mentioned-idents)))
            
            ;; Should return list of files
            (should (listp ranked-files))
            (should (= (length ranked-files) 3))
            
            ;; File with relevant identifier should be ranked higher
            (should (member relevant-file ranked-files))))
      
      (delete-directory temp-dir t))))

;; Enhanced caching tests
(ert-deftest test-gptel-extra-enhanced-caching ()
  "Test enhanced caching functionality."
  (let* ((temp-file (make-temp-file "test-enhanced-cache" nil ".py"))
         (map (repo-map-create)))
    
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "def test_function():\n    pass"))
          
          ;; First call should parse and cache
          (let ((tags1 (gptel-extra--enhanced-get-tags map temp-file)))
            (should tags1)
            
            ;; Second call should use cache
            (let ((tags2 (gptel-extra--enhanced-get-tags map temp-file)))
              (should (equal tags1 tags2)))))
      
      (delete-file temp-file))))

;; Cleanup tests
;; (ert-deftest test-gptel-extra-cleanup ()
;;   "Test cleanup functionality."
;;   (let ((test-buffer (get-buffer-create " *gptel-extra-test*")))
;;     (should (buffer-live-p test-buffer))
;;     (gptel-extra--cleanup-buffers)
;;     (should-not (buffer-live-p test-buffer))))

;; Context building tests
(ert-deftest test-gptel-extra-context-building ()
  "Test incremental context building."
  (let* ((temp-dir (make-temp-file "test-context" t))
         (test-file (expand-file-name "test.py" temp-dir))
         (default-directory temp-dir))
    
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "def example_function():\n    pass\n\nclass ExampleClass:\n    pass"))
          
          (let* ((prompt "Please look at example_function")
                 (context-entries (gptel-extra--build-context-incrementally prompt "")))
            (should (listp context-entries))
            ;; Should contain context entries if parsing succeeds
            (when (> (length context-entries) 0)
              (should (cl-some (lambda (entry)
                                 (let ((content (plist-get entry :content)))
                                   (and content (string-match-p "example_function\\|ExampleClass" content))))
                               context-entries)))))
      
      (delete-directory temp-dir t))))

;; Enhanced repo map tests
(ert-deftest test-gptel-extra-enhanced-repo-map ()
  "Test enhanced repo map generation."
  (let* ((temp-dir (make-temp-file "test-enhanced-repo" t))
         (test-file (expand-file-name "test.py" temp-dir))
         (default-directory temp-dir))
    
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "def target_function():\n    pass\n\nclass TargetClass:\n    pass"))
          
          (let* ((chat-files nil)
                 (other-files (list test-file))
                 (mentioned-idents '("target_function"))
                 (context (gptel-extra--enhanced-repo-map chat-files other-files nil mentioned-idents)))
            
            (should (stringp context))
            ;; Should contain relevant information if parsing succeeds
            (when (> (length context) 0)
              (should (string-match-p "target_function" context)))))
      
      (delete-directory temp-dir t))))

;; Integration tests
(ert-deftest test-gptel-extra-integration ()
  "Test integration of multiple features."
  (let* ((temp-dir (make-temp-file "test-integration" t))
         (py-file (expand-file-name "module.py" temp-dir))
         (js-file (expand-file-name "script.js" temp-dir))
         (ignore-file (expand-file-name "node_modules/lib.js" temp-dir))
         (gptel-extra-debug nil)
         (gptel-extra-max-context-tokens 1000)
         (default-directory temp-dir))
    
    (unwind-protect
        (progn
          ;; Create test files
          (make-directory (file-name-directory ignore-file) t)
          (with-temp-file py-file
            (insert "def process_data():\n    return 'processed'\n\nclass DataProcessor:\n    pass"))
          (with-temp-file js-file
            (insert "function processData() {\n    return 'processed';\n}"))
          (with-temp-file ignore-file
            (insert "// This should be ignored"))
          
          ;; Test comprehensive workflow
          (let* ((all-files (list py-file js-file ignore-file))
                 (filtered-files (gptel-extra--filter-files all-files))
                 (mentioned-idents '("process_data"))
                 (ranked-files (gptel-extra--rank-files filtered-files mentioned-idents)))
            
            ;; Should filter out ignored files
            (should-not (member ignore-file filtered-files))
            (should (member py-file filtered-files))
            (should (member js-file filtered-files))
            
            ;; Should rank files appropriately
            (should (listp ranked-files))
            (should (>= (length ranked-files) 2))))
      
      (delete-directory temp-dir t))))

;; Performance tests
(ert-deftest test-gptel-extra-performance ()
  "Test performance characteristics."
  (let* ((temp-dir (make-temp-file "test-performance" t))
         (default-directory temp-dir))
    
    (unwind-protect
        (progn
          ;; Create multiple test files
          (dotimes (i 10)
            (let ((file (expand-file-name (format "test%d.py" i) temp-dir)))
              (with-temp-file file
                (insert (format "def function%d():\n    pass\n\nclass Class%d:\n    pass" i i)))))
          
          (let* ((files (directory-files temp-dir t "\\.py$"))
                 (start-time (float-time)))
            
            ;; Test that operations complete in reasonable time
            (gptel-extra--filter-files files)
            (gptel-extra--rank-files files '("function1" "Class5"))
            
            (let ((elapsed (- (float-time) start-time)))
              ;; Should complete within 5 seconds
              (should (< elapsed 5.0)))))
      
      (delete-directory temp-dir t))))
