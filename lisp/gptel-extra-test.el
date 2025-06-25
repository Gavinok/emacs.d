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

(ert-deftest test-aider-coder-creation ()
  (let ((coder (aider-init)))
    (should (object-of-class-p coder 'aider-coder))
    (should (equal (oref coder :model) "gpt-4"))
    (should (equal (oref coder :edit-format) "diff"))
    (should (= (oref coder :map-tokens) 1024))
    (should (equal (oref coder :verbose) t))
    (should (equal (oref coder :stream) t))
    (should (null (oref coder :auto-commits)))
    (should (null (oref coder :dirty-commits)))))

(ert-deftest test-aider-coder-file-paths ()
  (let* ((coder (aider-init))
         (test-path "test/file.el"))
    (should (equal (get-rel-fname coder (expand-file-name test-path)) test-path))
    (should (equal (file-name-nondirectory
                    (get-abs-fname coder test-path)) "file.el"))))

(ert-deftest test-aider-coder-file-operations ()
  (let* ((coder (aider-init))
         (temp-file (make-temp-file "test")))
    (unwind-protect
        (progn
          (add-file coder temp-file)
          (should (member temp-file (oref coder files)))
          (remove-file coder temp-file)
          (should-not (member temp-file (oref coder files))))
      (delete-file temp-file))))

(ert-deftest test-aider-coder-file-content ()
  (let* ((coder (aider-init))
         (temp-file (make-temp-file "test"))
         (test-content "test content"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert test-content))
          (should (equal (get-file-content coder temp-file) test-content)))
      (delete-file temp-file))))

(ert-deftest test-aider-coder-apply-edit ()
  (let* ((coder (aider-init))
         (temp-file (make-temp-file "test"))
         (new-content "new content"))
    (unwind-protect
        (progn
          (apply-edit coder temp-file new-content)
          (should (equal (get-file-content coder temp-file) new-content)))
      (delete-file temp-file))))


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
