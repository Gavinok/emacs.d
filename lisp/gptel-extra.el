;;; gptel-extra.el --- Core RepoMap functionality for aiding development in Emacs -*- lexical-binding: t -*-
;;
;; Author: [Your Name]
;; Version: 0.1.0
;; License: MIT
;; Created: [Creation Date]
;;
;; This file provides functionality for managing a repository map,
;; extracting code identifiers and file mentions, and facilitating editing
;; and tracking of files within an Emacs environment augmented with
;; AI assistance.
;;
;; Usage:
;; Load this file and create an instance of `aider-coder` to begin
;; using its features.
;;
;;; Commentary:
;;
;; The repository map helps in understanding project structure
;; and aids in efficient code navigation and editing.
;;
;;; Code:
(require 'treesit)
(require 'filenotify)
(require 'cl-lib)
(require 'gptel-transient)

;;; repo map generation
(defvar repo-map-cache-version 4
  "Version of the repo map cache format.")

(defvar repo-map-cache-dir (expand-file-name ".aider.tags.cache.v4" default-directory)
  "Directory for storing repo map cache files.")

(defvar repo-map-warned-files nil
  "Set of files that have triggered warnings.")

(cl-defstruct repo-map-tag
  rel-fname    ; Relative filename
  fname        ; Full filename
  line         ; Line number
  beg          ; beginning of region
  end          ; end of region
  buffer       ; buffer
  name         ; Symbol name
  content      ; the content of this tag
  kind)        ; Type of tag (def/ref)

(cl-defstruct repo-map
  root              ; Root directory
  max-map-tokens    ; Maximum tokens in map
  cache-threshold   ; Cache threshold
  tree-cache        ; Cache for parsed trees
  map-cache         ; Cache for generated maps
  last-map          ; Last generated map
  verbose)          ; Verbose output flag

(defun repo-map-create (&optional root)
  "Create a new repository map instance."
  (make-repo-map
   :root (or root default-directory)
   :max-map-tokens 1024
   :cache-threshold 0.95
   :tree-cache (make-hash-table :test 'equal)
   :map-cache (make-hash-table :test 'equal)
   :verbose nil))

(defun repo-map-get-rel-fname (map fname)
  "Get relative filename from MAP's root."
  (file-relative-name fname (repo-map-root map)))

(defun repo-map-get-mtime (fname)
  "Get modification time of FNAME."
  (when (file-exists-p fname)
    (float-time (nth 5 (file-attributes fname)))))

(defun repo-map-get-tags (map fname)
  "Get tags from FNAME using tree-sitter."
  (let* ((rel-fname (repo-map-get-rel-fname map fname))
         (file-mtime (repo-map-get-mtime fname))
         (cache-key fname)
         cached-val)
    (when file-mtime
      ;; Check cache first
      (setq cached-val (gethash cache-key (repo-map-tree-cache map)))
      (if (and cached-val
               (equal (plist-get cached-val :mtime) file-mtime))
          (plist-get cached-val :data)
        ;; Cache miss - parse file
        (let ((tags (repo-map--parse-file-tags map fname rel-fname)))
          ;; Update cache
          (puthash cache-key
                   (list :mtime file-mtime :data tags)
                   (repo-map-tree-cache map))
          tags)))))

(defun force-ts-mode (buffer)
  "Force switch to tree-sitter mode if a compatible one exists.
BUFFER is the buffer to switch modes in.
Returns the BUFFER."
  (with-current-buffer buffer
    (unless treesit-font-lock-feature-list
      (pcase major-mode
        ('go-mode (go-ts-mode))
        ('python-mode (python-ts-mode))
        (_ (error "No matching tree-sitter mode setup for %s" major-mode)))))
  buffer)

(defun repo-map--parse-file-tags (_map fname rel-fname)
  "Parse tags from FNAME using tree-sitter."
  (condition-case err
      (when-let* ((code-buf (force-ts-mode (find-file-noselect fname)))
                  (lang (with-current-buffer code-buf
                          (treesit-language-at (point))))
                  (tree (with-current-buffer code-buf
                          (treesit-buffer-root-node lang))))
        ;; Query tree for definitions and references
        (let (tags
              (captures (treesit-query-capture tree
                                               (pcase lang
                                                 ('python '((function_definition name: (identifier) @def)
                                                            (class_definition name: (identifier) @def)))
                                                 ('go     '((function_declaration name: (identifier) @def)
                                                            (method_declaration receiver: (_) name: (field_identifier) @def)))
                                                 ('rust   '((function_item (identifier) @def)))
                                                 )
                                               )))
          ;; Add definition tags
          (dolist (capture captures)
            (push (make-repo-map-tag
                   :rel-fname rel-fname
                   :fname fname
                   :line (with-current-buffer code-buf
                           (line-number-at-pos (treesit-node-start (cdr capture))))
                   :beg (treesit-node-start (treesit-node-parent (cdr capture)))
                   :end (treesit-node-end (treesit-node-parent (cdr capture)))
                   :buffer (treesit-node-buffer (cdr capture))
                   :content (cl-reduce (lambda (a b) (concat a " " b)) (seq-map #'treesit-node-text (treesit-node-children (treesit-node-parent (cdr capture)))))
                   :name (treesit-node-text (cdr capture))
                   :kind (car capture))
                  tags))
          ;; Add reference tags similarly
          tags))
    (error (message "failed with error %s" err)
           nil)))
;; TODO: Additional helper functions woul//d be needed for:
;; - Ranking tags by importance
;; - Converting tags to tree representation
;; - Managing caches
;; - Generating final map output
;; - skip git ignored files
;; - support possibly pulling mentioned urls into the context
;; - add caching
;;   - Only parse new or changed files
;;   - Only need to check the chat context on new key words
;; - add default most changed files git log --pretty=format: --name-only | grep -v '^$' | sort | uniq -c | sort -nr | head -n 10
;; - add support for checking compilation buffer and flymake for errors
;; - integrate file location with claude specific referencing https://github.com/wasabeef/yank-for-claude.nvim

;; NOTE: We need to rank the identifiers/ symbols and files
;; Inspired by https://github.com/Aider-AI/aider/blob/17d40a62/aider/repomap.py#L468-L479
;; here is how we could rank identifiers
;; Mentioned Identifiers in chat: 10x (currently the major way we pick things out)
;; Snake or camel cased with over 8 letters
;; Private symbols: .1x (requires us to check if they are exported) Or just starts with _
;; Referenced by other code: 50x Not hunted down yet. Could maybe use xref for this. Should 100% be cached
;; Symbol that is defined in multiple files: .1x (since this will probably produce way too many conflicting results)
;; mentioned in highly edited file (git history/last edit) 2x

;;; Base editor functionality
(require 'json)
(require 'subr-x)
(require 'project)
(require 'eieio)
(defvar aider-version "0.1.0")
(defvar aider-model "gpt-4")
(defvar aider-edit-format "diff")
(defvar aider-max-tokens 1024)

(defclass aider-coder ()
  ((io :initarg :io :accessor io)
   (model :initarg :model :accessor model)
   (edit-format :initarg :edit-format :accessor edit-format :initform aider-edit-format)
   (root :initarg :root :accessor root :initform default-directory)
   (files :initarg :files :accessor files :initform nil)
   (read-only-files :initarg :read-only-files :accessor read-only-files :initform nil)
   (repo :initarg :repo :accessor repo)
   (map-tokens :initarg :map-tokens :accessor map-tokens :initform aider-max-tokens)
   (verbose :initarg :verbose :accessor verbose)
   (stream :initarg :stream :accessor stream)
   (auto-commits :initarg :auto-commits :accessor auto-commits :initform nil)
   (dirty-commits :initarg :dirty-commits :accessor dirty-commits :initform nil)
   (dry-run :initarg :dry-run :accessor dry-run :initform nil)
   (show-diffs :initarg :show-diffs :accessor show-diffs :initform nil)))

(cl-defmethod initialize-instance ((coder aider-coder) &rest slots)
  (cl-call-next-method))

(cl-defmethod get-rel-fname ((coder aider-coder) fname)
  "Get relative filename from absolute path."
  (file-relative-name fname (oref coder root)))

(cl-defmethod get-abs-fname ((coder aider-coder) fname)
  "Get absolute filename from relative path."
  (expand-file-name fname (oref coder root)))

(cl-defmethod add-file ((coder aider-coder) fname)
  "Add a file to be tracked."
  (let ((abs-fname (get-abs-fname coder fname)))
    (when (file-exists-p abs-fname)
      (cl-pushnew abs-fname (oref coder files)))))

(cl-defmethod remove-file ((coder aider-coder) fname)
  "Remove a file from being tracked."
  (let ((abs-fname (get-abs-fname coder fname)))
    (setf (oref coder files)
          (remove abs-fname (oref coder files)))))

(cl-defmethod get-file-content ((coder aider-coder) fname)
  "Get content of a file."
  (with-temp-buffer
    (insert-file-contents fname)
    (buffer-string)))

(cl-defmethod apply-edit ((coder aider-coder) fname content)
  "Apply edit to a file."
  (unless (oref coder dry-run)
    (with-temp-file fname
      (insert content))))

(cl-defmethod commit-changes ((coder aider-coder) files msg)
  "Commit changes to files."
  (when (and (oref coder repo)
             (not (oref coder dry-run)))
    (vc-checkin files 'Git msg)))

;; Usage example:
(defun aider-init ()
  "Initialize aider coder."
  (let ((coder (make-instance 'aider-coder
                              :root default-directory
                              :files '()
                              :model "gpt-4"
                              :edit-format "diff"
                              :map-tokens 1024
                              :verbose t
                              :stream t
                              :auto-commits nil
                              :dirty-commits nil)))
    coder))

;;; Mention hunter
(defun get-file-mentions (text)
  "Extract potential file mentions from TEXT.
Returns a set of normalized file path mentions."
  (let ((mentions (make-hash-table :test 'equal))
        (words (split-string text "[][ \t\n\r\"'`,(){}<>!?:;-]+" t)))

    ;; Check each word for file-like patterns
    (dolist (word words)
      (when (and (> (length word) 3) ; Skip very short words
                 (or (string-match-p "\\." word) ; Has extension
                     (string-match-p "/" word)    ; Has path separator
                     (string-match-p "\\\\" word))) ; Windows path

        ;; Normalize the path
        (let ((norm-path (file-name-sans-versions
                          (replace-regexp-in-string "^\\./" "" word))))
          (puthash norm-path t mentions))))

    ;; Convert hash to list
    (let (result)
      (maphash (lambda (k _v) (push k result)) mentions)
      result)))

(defun get-ident-mentions (text)
  "Extract likely code identifiers from TEXT.
Returns a set of identifier mentions."
  (let ((mentions (make-hash-table :test 'equal))
        (words (split-string text "[][ \t\n\r\"'`.,(){}/<>!?:;-]+" t)))

    ;; Check each word
    (dolist (word words)
      (when (and (> (length word) 2) ; Skip very short words
                 ;; Basic identifier pattern check
                 (string-match-p "^[a-zA-Z_][a-zA-Z0-9_]*$" word)
                 ;; Skip common English words and single letters
                 (not (member word '("function" "the" "new" "and" "for" "you" "this" "that"))))

        (puthash word t mentions)))

    ;; Convert hash to list
    (let (result)
      (maphash (lambda (k _v) (push k result)) mentions)
      result)))

(defun get-ident-filename-matches (identifiers)
  "Get matches of IDENTIFIERS with their corresponding filenames in the repository.
Returns a list of cons cells where each cell contains an identifier and its associated filename."
  (let ((matches nil))
    (dolist (identifier identifiers)
      ;; Assuming we have a function to find filenames associated with the identifier.
      (let ((filenames (mapcar 'car (get-rankings-for-identifier identifier)))) ; Placeholder function
        (dolist (fname filenames)
          (push (cons identifier fname) matches)))))
  matches)

(defun get-rankings-for-identifier (identifier)
  "Placeholder for a function that retrieves file names where IDENTIFIER is used.
This function should return a list of cons cells containing the filename and related information."
  ;; This should be the implementation retrieving information based on the identifier
  ;; For now, let's return a dummy list to satisfy logic flow.
  (list (cons "example_file.el" 1)
        (cons "another_file.el" 2)))

(defun gptel-generate-repo-map (chat-files other-files &optional mentioned-files mentioned-idents)
  "Generate a repository map for the current project context.
CHAT-FILES are files currently being discussed/edited.
OTHER-FILES are other project files to consider.
MENTIONED-FILES are files mentioned in the conversation.
MENTIONED-IDENTS are identifiers mentioned in the conversation."
  (let* ((map (repo-map-create default-directory))
         (ranked-tags nil)
         (chat-rel-fnames (mapcar (lambda (f) (file-relative-name f (repo-map-root map)))
                                  chat-files))
         (map-tokens 1024) ;; Default token limit
         (files-listing ""))

    ;; Get ranked tags using tree-sitter
    (setq ranked-tags
          (mapcar (lambda (fname)
                    (when-let* ((rel-fname (file-relative-name fname (repo-map-root map)))
                                (tags (or (repo-map-get-tags map rel-fname)
                                          t))
                                )
                      (cons (file-relative-name fname (repo-map-root map)) (repo-map-get-tags map rel-fname))))
                  (append chat-files other-files)))

    ;; Convert tags to tree representation
    (setq files-listing
          (with-temp-buffer
            (dolist (file-tags ranked-tags)
              (let ((fname (car file-tags))
                    (tags (cdr file-tags)))
                (when  (and (or (cl-member fname mentioned-files :test #'equal)
                                (cl-intersection (mapcar #'repo-map-tag-name tags) mentioned-idents :test #'equal))
                            (not (cl-member fname chat-rel-fnames :test #'equal)))
                  (insert "\n" fname ":\n")
                  ;; Insert file content snippet around each tag
                  (dolist (tag (seq-filter (lambda (tag) (member (repo-map-tag-name tag) mentioned-idents)) tags))
                    (when-let* ((line (repo-map-tag-line tag))
                                (name (repo-map-tag-name tag))
                                (kind (repo-map-tag-kind tag))
                                (content (repo-map-tag-content tag)))
                      (insert (format "  %s %s (line %d)\n%s\n...\n"
                                      kind name line content)))))))
            (cons (buffer-string)
                  ranked-tags)))

    ;; Return the formatted repo map
    files-listing))

(defun gptel-generate-repo-map-for-prompt (prompt chat-files)
  "Generate a repository map based on the PROMPT content.
CHAT-FILES are the files already added to the current context.
Extracts file and identifier mentions from the prompt to create a focused map."
  (let* ((mentioned-files (get-file-mentions prompt))
         (mentioned-idents (get-ident-mentions prompt))
         (p (project-current))
         (project-files (if p
                            (project-files p)
                          (directory-files-recursively default-directory ".*")))
         (chat-files chat-files)
         (other-files (seq-remove
                       (lambda (f) (member f chat-files))
                       project-files)))

    (gptel-generate-repo-map
     chat-files
     other-files
     mentioned-files
     mentioned-idents)))

(defun gptel-extra--get-context ()
  (remove nil (mapcar (lambda (c) (buffer-file-name (car c)))
                      gptel-context--alist)))
(defun gptel-extra-add-repo-map (chat-buffer)
  (interactive (list (current-buffer)))
  (let ((map (with-current-buffer chat-buffer
               (gptel-generate-repo-map-for-prompt (buffer-string)
                                                   (gptel-extra--get-context)))))
    (cl-loop for (file tag) in (cdr map)
             when tag
             do (with-current-buffer (find-file-noselect file)
                  (set-mark (repo-map-tag-beg tag))
                  (goto-char (repo-map-tag-end tag))
                  (setq-local mark-active t)
                  (gptel-add)))
    ;; (with-current-buffer (get-buffer-create "*gptel-repo-map*")
    ;;   (erase-buffer)
    ;;   (insert (car map))
    ;;   (gptel-add))
    ))

(defun my/next-import-by-lang ()
  "Depending on the current major mode search for the next import in the current file."
  (pcase major-mode
    ((or 'python-ts-mode 'python-mode) (treesit-search-forward-goto (treesit-node-at (point)) "\\(module_name\\|dotted_name\\)"
                                                                    'end))
    ((or 'typescript-ts-mode 'tsx-ts-mode vue-ts-mode) (treesit-search-forward-goto (treesit-node-at (point)) "\\(import_clause\\|import_specifier\\)" 'end))
    ;; TODO support Golang and more languages
    ;; ('go-ts-mode  (treesit-search-forward-goto (treesit-node-at (point)) "import_spec" 'end))
    ))

(defun gptel-extra-add-related-files-to-context ()
  "Find all imports in the current file and add them to gptel's context."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (seq-filter (lambda (f) (not (equal "" f)))
                (cl-loop while (my/next-import-by-lang)
                         do (condition-case err
                                (let ((f (xref-file-location-file
                                          (xref-match-item-location
                                           (car (funcall (xref--create-fetcher
                                                          (symbol-at-point)
                                                          'definitions
                                                          (symbol-at-point))))))))
                                  (message "Added %s to context"f)
                                  (gptel-add-file f))
                              (error (message "Error: %s when adding to context"
                                              (error-message-string err))))))))

(require 'transient)
(defun my/add-file (file)
  (with-current-buffer (find-file-noselect (file-relative-name file))
    (gptel-add)
    (message "%s added" (buffer-file-name (current-buffer)))))
(defun find-dup (output file-counts)
  (dolist (file (split-string output "\n" t))
    (puthash file (1+ (gethash file file-counts 0)) file-counts)))
(defun gptel-extra--add-most-modified-files ()
  (interactive)
  (let ((output (shell-command-to-string "git log --pretty=format: --name-only"))
        (file-counts (make-hash-table :test 'equal)))
    (find-dup output file-counts)
    (let ((file-list (hash-table-keys file-counts)))
      (setq file-list (sort file-list (lambda (a b)
                                        (> (gethash a file-counts)
                                           (gethash b file-counts)))))
      (dolist (file (cl-subseq file-list 0 (min 10 (length file-list))))
        (my/add-file file)))))

(transient-define-prefix gptel-extra-transient-prefix ()
  "My Custom Transient Command."
  [["Smart Context"
    :if (lambda () gptel-mode)
    ("a" "Add Context Based On Chat" (lambda () (interactive) (gptel-extra-add-repo-map (current-buffer))))]
   ["Add Related Files"
    :if (lambda () (my/next-import-by-lang))
    ("a" "Add all imported files to context" my/gptel-add-related-files-to-context)
    ]
   ["Modified Files"
    ("m" "Add the most frequently modified files to context" gptel-extra--add-most-modified-files)
    ]])

(setq gptel-extra-add-default-bindings t)

(when gptel-extra-add-default-bindings
  (transient-insert-suffix 'gptel-menu "C" '(" E" "Gptel Extra" gptel-extra-transient-prefix)))


(provide 'gptel-extra)
;;; gptel-extra.el ends here
