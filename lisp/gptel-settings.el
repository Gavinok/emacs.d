(gptel-make-anthropic "Claude"
  :stream t
  :key (gptel-api-key-from-auth-source "api.anthropic.com"))
(gptel-make-gemini "Gemini"
  :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")
  :stream t)
(gptel-make-openai "OpenRouter"               ;Any name you want
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key "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"
  :models '(deepseek/deepseek-r1:free
            mistralai/devstral-small:free
            deepseek/deepseek-chat-v3-0324:free
            mistralai/devstral-small
            meta-llama/llama-4-maverick
            google/gemini-2.5-pro-preview))
(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*** ")
(setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
(defun get-first-line-as-string (filename)
  "Return the first line of the specified FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((first-line (buffer-substring-no-properties
                       (point-min) (line-end-position))))
      (string-trim first-line))))
(setq gptel-org-branching-context t)
(setq my/gptel-base-directive (concat
                               "The current date is "
                               (format-time-string (car org-timestamp-formats) (current-time))
                               ". The current operating system is " (shell-command-to-string "uname -a") " " (get-first-line-as-string "/etc/os-release")))
(setq my/gptel-directives
      '((default . "You are a large language model living in Emacs and a helpful assistant.
Respond concisely. Never use org headings in your response. Always put newlines between list elements.")
        (org . "You are a large language model living in Emacs and a helpful assistant.
Respond concisely. Never use org headings in your response. Always put newlines between list elements.
Use orgmode source blocks for all code output. Only use valid org markup.

Links to sections of a file are formatted as file:filename::text on that line
e.g. if we are in a file called Work.org and want to linke to the line
```
 Type Theory
```
The link would look like
[[file:./Work.org:: Type Theory]]

Links to websites simply contain the url
e.g. [[https://google.com]]
")
        (writing     . "You are a Professional Writer assisting me with my writing. Respond concisely.")))

(setopt gptel-directives (mapcar (lambda (d)
                                   (cons (car d)
                                         (concat (cdr d)
                                                 "\n"
                                                 my/gptel-base-directive)))
                                 my/gptel-directives))
(setq gptel-tools
      (list
       (gptel-make-tool
        :function #'my/gptel--edit_file
        :name "edit_file"
        :description "Edit file with a list of edits, each edit contains a line-number,
         a old-string and a new-string, new-string will replace the old-string at the specified line.
         Only use this when explicitly told to."
        :args (list '(:name "file-path"
                            :type string
                            :description "The full path of the file to edit")
                    '(:name "file-edits"
                            :type array
                            :items (:type object
                                          :properties
                                          (:line_number
                                           (:type integer :description "The line number of the file where edit starts.")
                                           :old_string
                                           (:type string :description "The old-string to be replaced.")
                                           :new_string
                                           (:type string :description "The new-string to replace old-string.")))
                            :description "The list of edits to apply on the file"))
        :category "filesystem")
       (gptel-make-tool
        :function (lambda (file &optional linenumber)
                    (with-temp-message (format "Displaying File %s on line %s" file linenumber)
                      (with-current-buffer (get-file-buffer file)
                        (switch-to-buffer-other-window (current-buffer))
                        (goto-line linenumber))))
        :name "show_file"
        :description "Display a file to the user"
        :args (list '( :name "file"
                       :type string
                       :description "The file to display to the user")
                    '( :name "line_number"
                       :type integer
                       :description "the line number to display if available"
                       :optional t))
        :category "filesystem")
       (gptel-make-tool
        :function (lambda (buffer)
                    (with-temp-message (format "Reading Buffer %s" buffer)
                      (with-current-buffer (get-buffer (string-trim buffer))
                        (buffer-string))))
        :name "read_buffer"
        :description (concat "Read the contents of an emacs buffer."
                             "Use this as a last resort as it ")
        :args (list '( :name "buffer"
                       :type string
                       :description "The name of the emacs buffer to read the contents of. "))
        :category "buffers")
       (gptel-make-tool
        :function (lambda ()
                    (with-temp-message "Getting Compilation Buffer Contents"
                      (let ((compilation-buffer-name-function
                             (or project-compilation-buffer-name-function
                                 compilation-buffer-name-function)))
                        (with-current-buffer (get-buffer (compilation-find-buffer))
                          (buffer-string)))))
        :name "get_compilation_errors"
        :description (concat "Return the contents of the compilation or test errors.")
        :args nil
        :category "errors")
       (gptel-make-tool
        :function (lambda ()
                    (with-temp-message "Listing Project Buffers"
                      (cl-reduce #'concat (mapcar (lambda (buf)
                                                    (with-current-buffer buf
                                                      (format "%s %s %s\n" (buffer-name buf)  major-mode (buffer-file-name buf))))
                                                  (project-buffers (project-current))))))
        :name "project_buffers"
        :description (concat "List all project related buffers indicating the "
                             "buffer name, buffer's current mode file path. "
                             "If no file is associated with a buffer then it is nil. "
                             "This is expected for compilation windows for example. "
                             "compilation-mode is the mode used for compiling code. ")
        :args nil
        :category "buffers")
       (gptel-make-tool
        :function (lambda (filepath)
                    (with-temp-message (format "Finding File %s" filepath)
                      (let ((dir (expand-file-name
                                  (project-root (project-current)))))
                        (shell-command-to-string
                         (format "find %s -type f -iname %s" dir (concat "*" filepath "*"))))))
        :name "search_for_file"
        :description (concat "Recursively search for files and directories matching a pattern. "
                             "Searches through all subdirectories from the starting path. The search "
                             "is case-insensitive and matches partial names. Returns full paths to all "
                             "matching items. Great for finding files when you don't know their exact location. "
                             "Only searches within allowed directories.")
        :args (list '(:name "filepath"
                            :type string
                            :description "Path to the file to read. Supports relative paths and ~."))
        :category "filesystem")
       (gptel-make-tool
        :function (lambda (filepath start end)
                    (with-temp-message (format "Reading file %s" filepath)
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name filepath))
                        (buffer-substring (goto-line start) (goto-line end)))))
        :name "read_file_section"
        :description (concat "Read a region of a file rather than the entire thing. "
                             "Prefer this over read_buffer and read_file as it is more efficient.")
        :args (list '( :name "file"
                       :type string
                       :description "The name of the emacs file to read the contents of. ")
                    '( :name "start"
                       :type integer
                       :description "The first line to read from")
                    '( :name "end"
                       :type integer
                       :description "The last line to read from"))
        :category "filesystem")
       (gptel-make-tool
        :function (lambda (filepath)
                    (with-temp-message (format "reading %s" filepath)
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name filepath))
                        (buffer-string))))
        :name "read_file"
        :description "Read and display the contents of a file"
        :args (list '(:name "filepath"
                            :type string
                            :description "Path to the file to read. Supports relative paths and ~."))
        :category "filesystem")
       (gptel-make-tool
        :function (lambda (directory)
                    (mapconcat #'identity
                               (directory-files directory)
                               "\n"))
        :name "list_directory"
        :description "List the contents of a given directory"
        :args (list '(:name "directory"
                            :type string
                            :description "The path to the directory to list"))
        :category "filesystem")
       (gptel-make-tool
        :function (lambda (path filename content)
                    (with-temp-message (format "Writing %s/%s with content %s" path filename content)
                      (let ((full-path (expand-file-name filename path)))
                        (with-temp-buffer
                          (insert content)
                          (write-file full-path))
                        (format "Created file %s in %s" filename path))))
        :name "create_file"
        :description "Create a new file with the specified content"
        :args (list '(:name "path"
                            :type string
                            :description "The directory where to create the file")
                    '(:name "filename"
                            :type string
                            :description "The name of the file to create")
                    '(:name "content"
                            :type string
                            :description "The content to write to the file"))
        :category "filesystem")
       (gptel-make-tool
        :function (lambda (command)
                    (with-temp-message (format "Running command: %s" command)
                      (shell-command-to-string command)))
        :name "run_command"
        :description "Run a command."
        :args (list
               '(:name "command"
                       :type "string"
                       :description "Command to run."))
        :category "command")
       (gptel-make-tool
        :function #'run_async_command
        :name "run_async_command"
        :description "Run an async command."
        :args (list
               '(:name "command"
                       :type "string"
                       :description "Command to run."))
        :category "command"
        :async t
        :include t)
       (gptel-make-tool
        :function (lambda (url)
                    (with-current-buffer (url-retrieve-synchronously url)
                      (goto-char (point-min))
                      (forward-paragraph)
                      (let ((dom (libxml-parse-html-region (point) (point-max))))
                        (run-at-time 0 nil #'kill-buffer (current-buffer))
                        (with-temp-buffer
                          (shr-insert-document dom)
                          (buffer-substring-no-properties (point-min) (point-max))))))
        :name "read_url"
        :description "Fetch and read the contents of a URL"
        :args (list '(:name "url"
                            :type string
                            :description "The URL to read"))
        :category "web")
       (gptel-make-tool
        :function #'brave-search-query
        :name "brave_search"
        :description "Perform a web search using the Brave Search API"
        :args (list '(:name "query"
                            :type string
                            :description "The search query string"))
        :category "web")
       ;; (gptel-make-tool
       ;;  :function (lambda (query)
       ;;              (with-temp-message (concat "Searching Email about " query)
       ;;                (shell-command-to-string
       ;;                 (concat "mu find -u -r " query))))
       ;;  :name "search_mail"
       ;;  :description "Search User Email"
       ;;  :args (list
       ;;         '(:name "Email Query"
       ;;                 :type string
       ;;                 :description "Search term query for email (keep this short)"))
       ;;  :category "email")
       ;; (gptel-make-tool
       ;;  :function (lambda (query)
       ;;              (with-temp-message (concat "Searching For Documents " query)
       ;;                (shell-command-to-string
       ;;                 (concat "recollq -C -n 0-30 " query))))
       ;;  :name "search_documents"
       ;;  :description "Search for documents and files on this computer"
       ;;  :args (list
       ;;         '(:name "Document Query"
       ;;                 :type string
       ;;                 :description "Search terms query for documents"))
       ;;  :category "filesystem")
       ;; (gptel-make-tool
       ;;  :function (lambda (regex)
       ;;              (let ((default-directory (project-root (project-current))))
       ;;                (with-temp-message (concat "Searching Codebase "
       ;;                                           default-directory)
       ;;                  (shell-command-to-string
       ;;                   (concat "rg " regex " " default-directory)))))
       ;;  :name "search_codebase"
       ;;  :confirm nil
       ;;  :description "Search for text in the current programming project"
       ;;  :args (list
       ;;         '(:name "Regex"
       ;;                 :type string
       ;;                 :description "The regex used to search the current codebase"))
       ;;  :category "filesystem")
       ))
(defun run_async_command (callback command)
  "Run COMMAND asynchronously and pass output to CALLBACK."
  (condition-case error
      (let ((buffer (generate-new-buffer " *async output*")))
        (with-temp-message (format "Running async command: %s" command)
          (async-shell-command command buffer nil))
        (let ((proc (get-buffer-process buffer)))
          (when proc
            (set-process-sentinel
             proc
             (lambda (process _event)
               (unless (process-live-p process)
                 (with-current-buffer (process-buffer process)
                   (let ((output (buffer-substring-no-properties (point-min) (point-max))))
                     (kill-buffer (current-buffer))
                     (funcall callback output)))))))))
    (t
     ;; Handle any kind of error
     (funcall callback (format "An error occurred: %s" error)))))
(defun brave-search-query (query)
  "Perform a web search using the Brave Search API with the given QUERY."
  (interactive "M")
  (let ((url-request-method "GET")
        (url-request-extra-headers `(("X-Subscription-Token" . ,(password-store-get "brave-api"))))
        (url (format "https://api.search.brave.com/res/v1/web/search?q=%s" (url-encode-url query))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil 'move)
        (let ((json-object-type 'hash-table)) ; Use hash-table for JSON parsing
          (cl-loop for i across (thread-first
                                  (json-parse-string (buffer-substring-no-properties (point) (point-max)))
                                  (map-elt  "web")
                                  (map-elt "results"))
                   collect (list :title (map-elt i "title")
                                 :description (map-elt i "description")))
          ))
      )))
(defun brave-summary-search-query (query)
  "Perform a web search using the Brave Search API with the given QUERY."
  (interactive "M")
  (let ((url-request-method "GET")
        (url-request-extra-headers `(("X-Subscription-Token" . ,(password-store-get "brave-api"))))
        (url (format "https://api.search.brave.com/res/v1/summarizer/search?q=%s" (url-encode-url query))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil 'move)
        (let ((json-object-type 'hash-table)) ; Use hash-table for JSON parsing
          (json-parse-string (buffer-substring-no-properties (point) (point-max))))))))

(defun my/gptel--edit_file (file-path file-edits)
  "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
  (if (and file-path (not (string= file-path "")) file-edits)
      (with-current-buffer (get-buffer-create "*edit-file*")
        (erase-buffer)
        (insert-file-contents (expand-file-name file-path))
        (let ((inhibit-read-only t)
              (case-fold-search nil)
              (file-name (expand-file-name file-path))
              (edit-success nil))
          ;; apply changes
          (dolist (file-edit (seq-into file-edits 'list))
            (when-let ((line-number (plist-get file-edit :line_number))
                       (old-string (plist-get file-edit :old_string))
                       (new-string (plist-get file-edit :new_string))
                       (is-valid-old-string (not (string= old-string ""))))
              (goto-char (point-min))
              (forward-line (1- line-number))
              (when (search-forward old-string nil t)
                (replace-match new-string t t)
                (setq edit-success t))))
          ;; return result to gptel
          (if edit-success
              (progn
                ;; show diffs
                (ediff-buffers (find-file-noselect file-name) (current-buffer))
                (format "Successfully edited %s" file-name))
            (format "Failed to edited %s" file-name))))
    (format "Failed to edited %s" file-path)))



(defun my/treesit-defun-at-point ()
  (require 'treesit)
  (treesit-node-first-child-for-pos (treesit-node-on (window-start)
                                                     (window-end)
                                                     (treesit-language-at (point)))
                                    (point)))

(defun my/identifiers-in-defun ()
  (narrow-to-region (treesit-node-start (my/treesit-defun-at-point))
                    (treesit-node-end (my/treesit-defun-at-point)))
  ;; (cl-loop with root = (my/treesit-defun-at-point)
  ;;          while (treesit-search-forward-goto root "identifier")
  ;;          collect (progn
  ;;                    (setq root (treesit-node-at (point)))
  ;;                    (thing-at-point 'symbol)))
  )

;; capture all instances of something in a buffer using a query. Examples at https://github.com/nvim-treesitter/nvim-treesitter/blob/42fc28ba918343ebfd5565147a42a26580579482/queries/python/highlights.scm
;; (treesit-query-capture (treesit-buffer-root-node (treesit-language-at (point))) '((parameters
;;                                                                                    (identifier) @variable.parameter)))
;; (treesit-query-capture (treesit-buffer-root-node (treesit-language-at (point))) '((call
;;                                                                                    function: (identifier) @function.call)
;;                                                                                   (call
;;                                                                                    function: (attribute
;;                                                                                               attribute: (identifier) @function.method.call))))


;; Integrating aider like features
;; https://deepwiki.com/search/what-sorta-context-does-aider_17b022fc-c745-4dcf-9cb9-6d1f3162c927


;; TODO
;; - build a repo map
;; - quickly get the most modified files in the current git repo
;; - implement a repo map budget
;;   - implement a ranking system to determine what is and isn't necessary to include
;;   - 
;; - come up with an api 
