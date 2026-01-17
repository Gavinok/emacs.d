(require 'treesit)
(require 'filenotify)
(require 'cl-lib)
(when (require 'gptel-transient nil 'noerror)
  ;; gptel-transient is available
  )
(require 'json)
(require 'subr-x)
(require 'project)
(require 'eieio)

(defconst gptel-extra-version "0.1.0")

;; Repository map structures
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
  "Get relative filename for FNAME using MAP's root."
  (file-relative-name fname (repo-map-root map)))

(defun repo-map-get-mtime (fname)
  "Return modification time of FNAME, or nil if it doesn't exist."
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
  "Switch BUFFER into a tree-sitter mode if available."
  (with-current-buffer buffer
    (unless treesit-font-lock-feature-list
      (pcase major-mode
        ('go-mode      (go-ts-mode))
        ('python-mode  (python-ts-mode))
        (_ (error "No tree-sitter mode for %s" major-mode)))))
  buffer)

(defun repo-map--parse-file-tags (_map fname rel-fname)
  "Parse definitions in FNAME (REL-FNAME) using tree-sitter."
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

;; Identifier and file mention extraction
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
  "Enhanced repo map generation with smart context management.
Uses all the new improvements: rate limiting, token counting,
file filtering, symbol ranking, and intelligent caching."
  (interactive (list (current-buffer)))
  (gptel-extra--rate-limit-check)
  (gptel-extra--log "info" "Starting enhanced repo map generation")

  (let* ((prompt-text (with-current-buffer chat-buffer (buffer-string)))
         (existing-context (gptel-extra--get-context))
         (files-added 0)
         (total-tokens 0))

    ;; Use the enhanced repo map generation
    (let ((map (with-current-buffer chat-buffer
                 (gptel-extra--enhanced-repo-map
                  existing-context  ; chat-files (already in context)
                  (let ((p (project-current)))
                    (gptel-extra--filter-files
                     (if p (project-files p)
                       (directory-files-recursively default-directory ".*"))))
                  (get-file-mentions prompt-text)
                  (get-ident-mentions prompt-text)))))

      (when (and map (> (length map) 0))
        (gptel-extra--log "info" "Generated repo map with %d characters" (length map))
        (message "Generated enhanced repo map (%d characters)" (length map))

        ;; Add the map as context if it's not too large
        (let ((map-tokens (gptel-extra--count-tokens map)))
          (if (< map-tokens gptel-extra-max-context-tokens)
              (progn
                (with-temp-buffer
                  (insert map)
                  (gptel-add))
                (setq files-added 1)
                (setq total-tokens map-tokens)
                (gptel-extra--log "info" "Added repo map to context (%d tokens)" map-tokens))
            (gptel-extra--log "warn" "Repo map too large (%d tokens), truncating" map-tokens)
            (let ((trimmed-result (gptel-extra--trim-to-token-limit map gptel-extra-max-context-tokens)))
              (with-temp-buffer
                (insert (car trimmed-result))
                (gptel-add))
              (setq files-added 1)
              (setq total-tokens (cdr trimmed-result))
              (message "Added truncated repo map to context (%d tokens)" total-tokens)))))

      (gptel-extra--log "info" "Repo map generation complete: %d items, %d tokens"
                        files-added total-tokens)
      (message "Enhanced repo map complete: %d items added (%d tokens)"
               files-added total-tokens))))

(defun gptel-extra--next-import-by-lang ()
  "Depending on the current major mode search for the next import in the current file."
  (pcase major-mode
    ((or 'python-ts-mode 'python-mode) (treesit-search-forward-goto (treesit-node-at (point)) "\\(module_name\\|dotted_name\\)"
                                                                    'end))
    ((or 'typescript-ts-mode 'tsx-ts-mode vue-ts-mode) (treesit-search-forward-goto (treesit-node-at (point)) "\\(import_clause\\|import_specifier\\)" 'end))
    ;; TODO support Golang and more languages
    ;; ('go-ts-mode  (treesit-search-forward-goto (treesit-node-at (point)) "import_spec" 'end))
    ))

(defun gptel-extra-add-related-files-to-context ()
  "Find imports in current buffer and add files to context."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (seq-filter (lambda (f) (not (equal "" f)))
                (cl-loop while (gptel-extra--next-import-by-lang)
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

(defun gptel-extra--find-dup (output file-counts)
  "Update FILE-COUNTS hash table with the count of each FILE in OUTPUT.

OUTPUT is a newline-separated string of file names.
FILE-COUNTS is a hash table that stores the number of appearances
of each file in OUTPUT."
  (dolist (file (split-string output "\n" t))
    (puthash file (1+ (gethash file file-counts 0)) file-counts)))

(defun gptel-extra--add-most-modified-files ()
  "Add most frequently modified files from git to context."
  (interactive)
  (let ((output (shell-command-to-string "git log --pretty=format: --name-only"))
        (root (string-clean-whitespace (shell-command-to-string "git rev-parse --show-toplevel")))
        (file-counts (make-hash-table :test 'equal)))
    (gptel-extra--find-dup output file-counts)
    (let ((file-list (hash-table-keys file-counts)))
      (setq file-list (sort file-list (lambda (a b)
                                        (> (gethash a file-counts)
                                           (gethash b file-counts)))))
      (dolist (file (cl-subseq file-list 0 (min 10 (length file-list))))
        (unless (string-match-p "lock\\." file)
          (gptel-add-file (concat root "/" file)))))))

(require 'transient)
(transient-define-prefix gptel-extra-transient-prefix ()
  "Enhanced GPT-EL Extra with Smart Context Management."
  [["🎯 Smart Context"
    :if (lambda () gptel-mode)
    ("a" "Smart Repo Map" (lambda () (interactive) (gptel-extra-add-repo-map (current-buffer))))
    ("p" "Smart Context from Prompt" gptel-extra-smart-context-from-prompt)
    ("c" "Smart Clear Context" gptel-extra-smart-clear-context)]
   ["📁 Smart Files"
    ("f" "Smart Add Files" gptel-extra-smart-add-files)
    ("r" "Smart Add Recent Files" gptel-extra-smart-add-recent-files)
    ("i" "Add Related Import Files" gptel-extra-add-related-files-to-context)
    ("m" "Add Most Modified Files" gptel-extra--add-most-modified-files)]
   ["🔍 Diagnostics"
    ("e" "Add Error Context" gptel-extra-smart-add-error-context)
    ("d" "Toggle Debug Logging" (lambda () (interactive)
                                  (setq gptel-extra-debug (not gptel-extra-debug))
                                  (message "GPTel Extra debug: %s"
                                           (if gptel-extra-debug "ON" "OFF"))))]
   ["⚙️ Settings"
    ("t" "Set Token Limit" (lambda () (interactive)
                             (let ((limit (read-number "Token limit: " gptel-extra-max-context-tokens)))
                               (setq gptel-extra-max-context-tokens limit)
                               (message "Token limit set to %d" limit))))
    ("s" "Set Rate Limit" (lambda () (interactive)
                            (let ((interval (read-number "Min interval (seconds): " gptel-extra-min-request-interval)))
                              (setq gptel-extra-min-request-interval interval)
                              (message "Rate limit set to %.2f seconds" interval))))]])

(setq gptel-extra-add-default-bindings t)

(when gptel-extra-add-default-bindings
  (transient-insert-suffix 'gptel-menu "C" '(" E" "Gptel Extra" gptel-extra-transient-prefix)))

;;; Improvements and new functionality

;; Configuration and customization
(defgroup gptel-extra nil
  "Enhanced GPTel functionality with smart context management."
  :group 'gptel
  :prefix "gptel-extra-")

(defcustom gptel-extra-debug nil
  "Enable debug logging for gptel-extra."
  :type 'boolean
  :group 'gptel-extra)

(defcustom gptel-extra-ignore-patterns
  '("\\.git/" "node_modules/" "__pycache__/" "\\.pyc$" "\\.o$" "\\.so$"
    "target/" "build/" "dist/" "\\.lock$" "eln-cache/" "elpa/" "auto-save-list/"
    "backups/" "undo-fu-session/" "transient/" "tramp")
  "Patterns to ignore when scanning files."
  :type '(repeat string)
  :group 'gptel-extra)

(defcustom gptel-extra-max-context-tokens 8192
  "Maximum number of tokens to include in context."
  :type 'integer
  :group 'gptel-extra)

(defcustom gptel-extra-min-request-interval 1.0
  "Minimum seconds between requests to avoid rate limiting."
  :type 'float
  :group 'gptel-extra)

;; Global state variables
(defvar gptel-extra--last-request-time nil
  "Time of the last request for rate limiting.")

;; Token counting and trimming
(defun gptel-extra--count-tokens (text)
  "Estimate token count for TEXT.
Uses a rough approximation of ~4 characters per token."
  (if (or (null text) (string-empty-p text))
      0
    (/ (length text) 4)))

(defun gptel-extra--trim-to-token-limit (content max-tokens)
  "Trim CONTENT to stay within MAX-TOKENS.
Returns trimmed content and actual token count."
  (let ((estimated-tokens (gptel-extra--count-tokens content)))
    (if (> estimated-tokens max-tokens)
        (let ((trimmed (substring content 0 (* max-tokens 4))))
          (cons trimmed max-tokens))
      (cons content estimated-tokens))))

(defun gptel-extra--filter-context-entries-by-tokens (entries token-limit)
  "Filter context ENTRIES to stay within TOKEN-LIMIT.
Returns list of entries sorted by priority, keeping highest priority entries."
  (let* ((sorted-entries (sort (copy-sequence entries)
                               (lambda (a b)
                                 (> (gptel-extra--context-entry-priority a)
                                    (gptel-extra--context-entry-priority b)))))
         (selected-entries '())
         (total-tokens 0))
    
    (dolist (entry sorted-entries)
      (let ((entry-tokens (gptel-extra--context-entry-tokens entry)))
        (when (<= (+ total-tokens entry-tokens) token-limit)
          (push entry selected-entries)
          (setq total-tokens (+ total-tokens entry-tokens)))))
    
    (gptel-extra--log "info" "Filtered %d entries to %d entries (%d tokens)"
                      (length entries) (length selected-entries) total-tokens)
    (reverse selected-entries)))

(defun gptel-extra--total-context-tokens (entries)
  "Calculate total tokens for list of context ENTRIES."
  (cl-reduce #'+ entries :key #'gptel-extra--context-entry-tokens :initial-value 0))

;; Rate limiting
(defun gptel-extra--rate-limit-check ()
  "Check if enough time has passed since last request."
  (when gptel-extra--last-request-time
    (let ((elapsed (- (float-time) gptel-extra--last-request-time)))
      (when (< elapsed gptel-extra-min-request-interval)
        (sleep-for (- gptel-extra-min-request-interval elapsed)))))
  (setq gptel-extra--last-request-time (float-time)))

;; Logging
(defun gptel-extra--log (level msg &rest args)
  "Log message at LEVEL with MSG and ARGS."
  (when gptel-extra-debug
    (let ((formatted-msg (apply #'format msg args)))
      (message "[gptel-extra:%s] %s" level formatted-msg))))

;; File hashing for cache validation
(defun gptel-extra--file-hash (fname)
  "Generate a content hash for file FNAME."
  (when (file-exists-p fname)
    (with-temp-buffer
      (insert-file-contents fname)
      (secure-hash 'md5 (current-buffer)))))

(defun gptel-extra--cache-valid-p (fname cache-entry)
  "Check if CACHE-ENTRY is still valid for FNAME."
  (and cache-entry
       (equal (gptel-extra--file-hash fname)
              (plist-get cache-entry :hash))))

;; Enhanced caching with file fingerprinting
(defun gptel-extra--get-cached-tags (map fname)
  "Get cached tags for FNAME from MAP, validating cache."
  (let* ((cache-key fname)
         (cached-val (gethash cache-key (repo-map-tree-cache map))))
    (when (and cached-val
               (gptel-extra--cache-valid-p fname cached-val))
      (plist-get cached-val :data))))

(defun gptel-extra--cache-tags (map fname tags)
  "Cache TAGS for FNAME in MAP with hash validation."
  (let ((cache-key fname)
        (file-hash (gptel-extra--file-hash fname)))
    (puthash cache-key
             (list :hash file-hash :data tags :timestamp (float-time))
             (repo-map-tree-cache map))))

;; File filtering
(defun gptel-extra--should-ignore-file-p (file)
  "Check if FILE should be ignored based on patterns."
  (cl-some (lambda (pattern)
             (string-match-p pattern file))
           gptel-extra-ignore-patterns))

(defun gptel-extra--filter-files (files)
  "Filter FILES removing ignored patterns."
  (seq-remove #'gptel-extra--should-ignore-file-p files))

;; Symbol ranking system
(defun gptel-extra--rank-symbol (symbol-name context-text)
  "Rank SYMBOL-NAME based on various criteria and CONTEXT-TEXT."
  (let ((score 1.0))
    (gptel-extra--log "debug" "Ranking symbol: %s" symbol-name)

    ;; Longer, descriptive names get higher scores
    (when (> (length symbol-name) 8)
      (setq score (* score 1.5)))

    ;; CamelCase or snake_case patterns
    (when (string-match-p "\\([A-Z][a-z]+\\)\\|\\([a-z]+_[a-z]+\\)" symbol-name)
      (setq score (* score 1.2)))

    ;; Private symbols get lower scores
    (when (string-prefix-p "_" symbol-name)
      (setq score (* score 0.1)))

    ;; Symbols mentioned in current context get higher scores
    (when (and context-text
               (string-match-p (regexp-quote symbol-name) context-text))
      (setq score (* score 10)))

    (gptel-extra--log "debug" "Symbol %s ranked with score: %.2f" symbol-name score)
    score))

(defun gptel-extra--rank-files (files mentioned-idents)
  "Rank FILES by relevance to MENTIONED-IDENTS."
  (let ((file-scores (make-hash-table :test 'equal)))
    (dolist (file files)
      (let ((score 1.0))
        ;; Recently modified files get higher scores
        (when-let* ((mtime (repo-map-get-mtime file))
                    (age (- (float-time) mtime)))
          (when (< age (* 24 60 60)) ; Modified within 24 hours
            (setq score (* score 2.0))))

        ;; Files mentioned in identifiers get higher scores
        (when (cl-some (lambda (ident)
                         (string-match-p (regexp-quote ident) file))
                       mentioned-idents)
          (setq score (* score 5.0)))

        (puthash file score file-scores)))

    ;; Sort files by score descending
    (sort files (lambda (a b)
                  (> (gethash a file-scores 0)
                     (gethash b file-scores 0))))))

;; Error recovery
(defun gptel-extra--with-error-recovery (func &rest args)
  "Execute FUNC with ARGS, providing error recovery."
  (condition-case err
      (apply func args)
    (error
     (gptel-extra--log "error" "Function %s failed: %s" func err)
     nil)))

;; Incremental context building
(defun gptel-extra--build-context-incrementally (prompt existing-context)
  "Build context incrementally, prioritizing most relevant content.
Returns list of context entries instead of concatenated string."
  (let* ((mentioned-files (get-file-mentions prompt))
         (mentioned-idents (get-ident-mentions prompt))
         (p (project-current))
         (project-files (if p (project-files p)
                          (directory-files-recursively default-directory ".*")))
         (filtered-files (gptel-extra--filter-files project-files))
         (priority-files (gptel-extra--rank-files filtered-files mentioned-idents))
         (context-entries '())
         (context-budget gptel-extra-max-context-tokens)
         (used-tokens 0))

    (gptel-extra--log "info" "Building context with %d files, budget: %d tokens"
                      (length priority-files) context-budget)

    ;; Add existing context as generated entry if provided
    (when (and existing-context (> (length existing-context) 0))
      (let ((entry (gptel-extra--create-generated-context-entry existing-context 10.0)))
        (push entry context-entries)
        (setq used-tokens (+ used-tokens (gptel-extra--context-entry-tokens entry)))))

    (dolist (file priority-files)
      (when (< used-tokens context-budget)
        (gptel-extra--log "debug" "Processing file: %s" file)
        (when-let* ((relevant-regions (gptel-extra--get-relevant-file-regions file mentioned-idents)))
          (dolist (region-info relevant-regions)
            (when (< used-tokens context-budget)
              (let* ((beg (car region-info))
                     (end (cdr region-info))
                     (priority (gptel-extra--calculate-region-priority file mentioned-idents beg end))
                     (entry (gptel-extra--create-file-context-entry file beg end priority))
                     (entry-tokens (gptel-extra--context-entry-tokens entry)))
                (when (< (+ used-tokens entry-tokens) context-budget)
                  (push entry context-entries)
                  (setq used-tokens (+ used-tokens entry-tokens))
                  (gptel-extra--log "debug" "Added %s [%d-%d] (%d tokens), total: %d"
                                    file beg end entry-tokens used-tokens))))))))

    (gptel-extra--log "info" "Built %d context entries with %d tokens"
                      (length context-entries) used-tokens)
    (reverse context-entries)))

(defun gptel-extra--get-relevant-file-regions (file mentioned-idents)
  "Get relevant regions from FILE based on MENTIONED-IDENTS.
Returns list of (BEG . END) pairs representing relevant regions."
  (condition-case err
      (let ((regions '())
            (file-size (nth 7 (file-attributes file))))
        
        ;; For now, return entire file as single region
        ;; TODO: Implement more sophisticated region detection using tree-sitter
        (when (and file-size (< file-size (* 1024 1024))) ; Skip files > 1MB
          (push (cons nil nil) regions)) ; nil,nil means entire file
        
        regions)
    (error 
     (gptel-extra--log "error" "Failed to get regions from %s: %s" file err)
     nil)))

(defun gptel-extra--calculate-region-priority (file mentioned-idents beg end)
  "Calculate priority score for a region in FILE from BEG to END.
Higher scores indicate higher priority."
  (let ((priority 1.0))
    
    ;; File name relevance
    (dolist (ident mentioned-idents)
      (when (string-match-p (regexp-quote ident) file)
        (setq priority (* priority 3.0))))
    
    ;; Recently modified files get higher priority
    (when-let* ((mtime (repo-map-get-mtime file))
                (age (- (float-time) mtime)))
      (when (< age (* 24 60 60)) ; Modified within 24 hours
        (setq priority (* priority 1.5))))
    
    priority))

(defun gptel-extra--get-relevant-snippets (file mentioned-idents)
  "Extract relevant snippets from FILE based on MENTIONED-IDENTS."
  (condition-case err
      (let* ((map (repo-map-create))
             (tags (repo-map-get-tags map file))
             (relevant-tags (when tags
                              (seq-filter
                               (lambda (tag)
                                 (member (repo-map-tag-name tag) mentioned-idents))
                               tags))))
        (when relevant-tags
          (with-temp-buffer
            (insert (format "File: %s\n" (file-relative-name file)))
            (dolist (tag relevant-tags)
              (insert (format "  %s %s (line %d)\n"
                              (repo-map-tag-kind tag)
                              (repo-map-tag-name tag)
                              (repo-map-tag-line tag)))
              (when-let* ((content (repo-map-tag-content tag)))
                (insert (format "    %s\n" content))))
            (buffer-string))))
    (error
     (gptel-extra--log "error" "Failed to get snippets from %s: %s" file err)
     nil)))

;; Cleanup functions
(defun gptel-extra--cleanup-buffers ()
  "Clean up temporary buffers created by gptel-extra."
  (dolist (buffer (buffer-list))
    (when (and (buffer-name buffer)
               (string-prefix-p " *gptel-extra-" (buffer-name buffer)))
      (kill-buffer buffer))))

(defun gptel-extra--cleanup-cache (map)
  "Clean up old cache entries from MAP."
  (let ((cutoff-time (- (float-time) 3600))) ; 1 hour ago
    (maphash (lambda (key value)
               (when (< (plist-get value :timestamp 0) cutoff-time)
                 (remhash key (repo-map-tree-cache map))))
             (repo-map-tree-cache map))))

;; Enhanced repo-map-get-tags with new caching
(defun gptel-extra--enhanced-get-tags (map fname)
  "Enhanced version of repo-map-get-tags with better caching."
  (let ((cached-tags (gptel-extra--get-cached-tags map fname)))
    (if cached-tags
        (progn
          (gptel-extra--log "debug" "Using cached tags for %s" fname)
          cached-tags)
      (gptel-extra--log "debug" "Parsing tags for %s" fname)
      (when-let* ((tags (gptel-extra--with-error-recovery
                         #'repo-map--parse-file-tags map fname
                         (repo-map-get-rel-fname map fname))))
        (gptel-extra--cache-tags map fname tags)
        tags))))

;; Enhanced repo map generation
(defun gptel-extra--enhanced-repo-map (chat-files other-files &optional mentioned-files mentioned-idents)
  "Enhanced version of gptel-generate-repo-map with improvements."
  (gptel-extra--rate-limit-check)

  (let* ((map (repo-map-create default-directory))
         (filtered-other-files (gptel-extra--filter-files other-files))
         (ranked-files (gptel-extra--rank-files filtered-other-files mentioned-idents))
         (context-budget gptel-extra-max-context-tokens)
         (used-tokens 0)
         (result-buffer (get-buffer-create " *gptel-extra-context*")))

    (gptel-extra--cleanup-cache map)

    (with-current-buffer result-buffer
      (erase-buffer)

      (dolist (file ranked-files)
        (when (< used-tokens context-budget)
          (let* ((rel-fname (file-relative-name file (repo-map-root map)))
                 (tags (gptel-extra--enhanced-get-tags map file))
                 (relevant-tags (when tags
                                  (seq-filter
                                   (lambda (tag)
                                     (member (repo-map-tag-name tag) mentioned-idents))
                                   tags))))

            (when (and relevant-tags
                       (not (member rel-fname
                                    (mapcar (lambda (f) (file-relative-name f))
                                            chat-files))))
              (let ((snippet (format "\n%s:\n" rel-fname)))
                (dolist (tag relevant-tags)
                  (setq snippet
                        (concat snippet
                                (format "  %s %s (line %d)\n%s\n...\n"
                                        (repo-map-tag-kind tag)
                                        (repo-map-tag-name tag)
                                        (repo-map-tag-line tag)
                                        (or (repo-map-tag-content tag) "")))))

                (let ((snippet-tokens (gptel-extra--count-tokens snippet)))
                  (when (< (+ used-tokens snippet-tokens) context-budget)
                    (insert snippet)
                    (setq used-tokens (+ used-tokens snippet-tokens)))))))))

      (gptel-extra--log "info" "Generated repo map with %d tokens" used-tokens)
      (buffer-string))))

;; Context entry creation and management

(defun gptel-extra--create-file-context-entry (file beg end &optional priority)
  "Create a context entry for FILE region from BEG to END.
PRIORITY is used for ranking (higher = more important)."
  (let* ((content (condition-case err
                      (with-temp-buffer
                        (insert-file-contents file nil beg end)
                        (buffer-string))
                    (error 
                     (gptel-extra--log "error" "Failed to read %s: %s" file err)
                     "")))
         (tokens (gptel-extra--count-tokens content)))
    (list :file file 
          :region (cons beg end)
          :content content
          :tokens tokens
          :priority (or priority 1.0))))

(defun gptel-extra--create-generated-context-entry (content &optional priority)
  "Create a context entry for generated CONTENT.
PRIORITY is used for ranking (higher = more important)."
  (let ((tokens (gptel-extra--count-tokens content)))
    (list :generated t
          :content content
          :tokens tokens
          :priority (or priority 1.0))))

(defun gptel-extra--context-entry-tokens (entry)
  "Get token count from context ENTRY."
  (plist-get entry :tokens))

(defun gptel-extra--context-entry-priority (entry)
  "Get priority from context ENTRY."
  (or (plist-get entry :priority) 1.0))

(defun gptel-extra--context-entry-content (entry)
  "Get content from context ENTRY."
  (plist-get entry :content))

(defun gptel-extra--apply-context-entries (entries)
  "Apply list of context ENTRIES to gptel using appropriate methods.
Returns total number of tokens applied."
  (let ((total-tokens 0)
        (entries-applied 0))
    
    (dolist (entry entries)
      (condition-case err
          (cond
           ;; File-based context entry
           ((plist-get entry :file)
            (let* ((file (plist-get entry :file))
                   (region (plist-get entry :region))
                   (beg (car region))
                   (end (cdr region)))
              (if (and beg end)
                  ;; Apply specific region
                  (save-excursion
                    (with-temp-buffer
                      (insert-file-contents file nil beg end)
                      ;; Set buffer's file name so gptel knows the file context
                      (setq buffer-file-name file)
                      (gptel-add)))
                ;; Apply entire file
                (gptel-add-file file))
              (setq total-tokens (+ total-tokens (gptel-extra--context-entry-tokens entry)))
              (setq entries-applied (1+ entries-applied))
              (gptel-extra--log "info" "Applied file context: %s [%s-%s] (%d tokens)"
                                file beg end (gptel-extra--context-entry-tokens entry))))
           
           ;; Generated content entry
           ((plist-get entry :generated)
            (let ((content (plist-get entry :content)))
              (with-temp-buffer
                (insert content)
                (gptel-add))
              (setq total-tokens (+ total-tokens (gptel-extra--context-entry-tokens entry)))
              (setq entries-applied (1+ entries-applied))
              (gptel-extra--log "info" "Applied generated context (%d tokens)"
                                (gptel-extra--context-entry-tokens entry))))
           
           (t
            (gptel-extra--log "warn" "Unknown context entry type: %s" entry)))
        
        (error 
         (gptel-extra--log "error" "Failed to apply context entry %s: %s" entry err))))
    
    (gptel-extra--log "info" "Applied %d context entries (%d total tokens)"
                      entries-applied total-tokens)
    total-tokens))

;; New smart context functions that use all improvements

(defun gptel-extra-smart-context-from-prompt (prompt &optional max-tokens)
  "Build smart context from PROMPT using all new improvements.
MAX-TOKENS limits the context size (defaults to `gptel-extra-max-context-tokens`)."
  (interactive "sPrompt: ")
  (let ((gptel-extra-max-context-tokens (or max-tokens gptel-extra-max-context-tokens)))
    (gptel-extra--rate-limit-check)
    (gptel-extra--log "info" "Building smart context from prompt")

    (let* ((context-entries (gptel-extra--build-context-incrementally prompt ""))
           (filtered-entries (gptel-extra--filter-context-entries-by-tokens 
                              context-entries gptel-extra-max-context-tokens))
           (total-tokens (gptel-extra--apply-context-entries filtered-entries)))

      (when (> total-tokens 0)
        (gptel-extra--log "info" "Added smart context (%d entries, %d tokens)" 
                          (length filtered-entries) total-tokens)
        (message "Added smart context to gptel (%d entries, %d tokens)" 
                 (length filtered-entries) total-tokens))

      filtered-entries)))

(defun gptel-extra-smart-add-files (files &optional mentioned-idents)
  "Intelligently add FILES to context using symbol ranking and filtering.
MENTIONED-IDENTS are identifiers to prioritize."
  (interactive "fFiles: ")
  (gptel-extra--rate-limit-check)
  (gptel-extra--log "info" "Smart adding files to context")

  (let* ((file-list (if (listp files) files (list files)))
         (filtered-files (gptel-extra--filter-files file-list))
         (ranked-files (gptel-extra--rank-files filtered-files
                                                (or mentioned-idents '())))
         (files-added 0)
         (total-tokens 0))

    (dolist (file ranked-files)
      (when (< total-tokens gptel-extra-max-context-tokens)
        (condition-case err
            (let* ((content (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string)))
                   (tokens (gptel-extra--count-tokens content)))
              (when (< (+ total-tokens tokens) gptel-extra-max-context-tokens)
                (gptel-add-file file)
                (setq files-added (1+ files-added))
                (setq total-tokens (+ total-tokens tokens))
                (gptel-extra--log "info" "Added %s (%d tokens)" file tokens)))
          (error (gptel-extra--log "error" "Failed to add %s: %s" file err)))))

    (gptel-extra--log "info" "Smart file addition complete: %d files, %d tokens"
                      files-added total-tokens)
    (message "Smart added %d files (%d tokens)" files-added total-tokens)))

(defun gptel-extra-smart-add-recent-files (&optional days)
  "Add recently modified files to context, filtered and ranked.
DAYS specifies how many days back to look (default 1)."
  (interactive "P")
  (let ((days (or days 1))
        (cutoff-time (- (float-time) (* days 24 60 60))))

    (gptel-extra--rate-limit-check)
    (gptel-extra--log "info" "Adding recent files (last %d days)" days)

    (let* ((p (project-current))
           (project-files (if p (project-files p)
                            (directory-files-recursively default-directory ".*")))
           (recent-files (seq-filter
                          (lambda (f)
                            (let ((mtime (repo-map-get-mtime f)))
                              (and mtime (> mtime cutoff-time))))
                          project-files))
           (filtered-files (gptel-extra--filter-files recent-files))
           (ranked-files (gptel-extra--rank-files filtered-files '()))
           (files-added 0)
           (total-tokens 0))

      (dolist (file (seq-take ranked-files 10)) ; Limit to top 10
        (when (< total-tokens gptel-extra-max-context-tokens)
          (gptel-extra--with-error-recovery
           (lambda ()
             (let* ((content (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string)))
                    (tokens (gptel-extra--count-tokens content)))
               (when (< (+ total-tokens tokens) gptel-extra-max-context-tokens)
                 (gptel-add-file file)
                 (setq files-added (1+ files-added))
                 (setq total-tokens (+ total-tokens tokens))
                 (gptel-extra--log "info" "Added recent file %s (%d tokens)" file tokens)))))))

      (gptel-extra--log "info" "Recent files addition complete: %d files, %d tokens"
                        files-added total-tokens)
      (message "Added %d recent files (%d tokens)" files-added total-tokens))))

(defun gptel-extra-smart-add-error-context ()
  "Add context from compilation errors and flymake diagnostics."
  (interactive)
  (gptel-extra--rate-limit-check)
  (gptel-extra--log "info" "Adding error context")

  (let ((context-parts '())
        (total-tokens 0))

    ;; Add compilation errors
    (when (get-buffer "*compilation*")
      (with-current-buffer "*compilation*"
        (let ((errors (buffer-substring-no-properties (point-min) (point-max))))
          (when (> (length errors) 0)
            (let ((tokens (gptel-extra--count-tokens errors)))
              (when (< (+ total-tokens tokens) gptel-extra-max-context-tokens)
                (push (format "Compilation Errors:\n%s\n" errors) context-parts)
                (setq total-tokens (+ total-tokens tokens))))))))

    ;; Add flymake diagnostics
    (when (and (fboundp 'flymake-diagnostics)
               (flymake-mode))
      (let ((diagnostics (flymake-diagnostics)))
        (when diagnostics
          (let ((diag-text (mapconcat
                            (lambda (d)
                              (format "%s:%d: %s: %s"
                                      (file-name-nondirectory
                                       (buffer-file-name (flymake-diagnostic-buffer d)))
                                      (line-number-at-pos (flymake-diagnostic-beg d))
                                      (flymake-diagnostic-type d)
                                      (flymake-diagnostic-text d)))
                            diagnostics "\n")))
            (let ((tokens (gptel-extra--count-tokens diag-text)))
              (when (< (+ total-tokens tokens) gptel-extra-max-context-tokens)
                (push (format "Flymake Diagnostics:\n%s\n" diag-text) context-parts)
                (setq total-tokens (+ total-tokens tokens))))))))

    (when context-parts
      (let ((full-context (string-join (reverse context-parts) "\n")))
        (with-temp-buffer
          (insert full-context)
          (gptel-add))
        (gptel-extra--log "info" "Added error context (%d tokens)" total-tokens)
        (message "Added error context (%d tokens)" total-tokens))
      total-tokens)))

(defun gptel-extra-smart-clear-context ()
  "Intelligently clear context, keeping only the most relevant parts."
  (interactive)
  (gptel-extra--log "info" "Smart clearing context")

  ;; Clear the gptel context
  (setq gptel-context--alist nil)

  ;; Clean up our caches
  (gptel-extra--cleanup-buffers)

  (gptel-extra--log "info" "Context cleared")
  (message "Context cleared intelligently"))

;; Add cleanup hook
(add-hook 'kill-emacs-hook #'gptel-extra--cleanup-buffers)

(provide 'gptel-extra)
;;; gptel-extra.el ends here
