;; init.el --- My config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; useful for quickly debugging Emacs
;; (setq debug-on-error t)
(setenv "LSP_USE_PLISTS" "true")
;;; Startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Make it easy to jump between packages
(setopt use-package-enable-imenu-support t)

;; Get clipboard to work when using wsl
(defconst running-in-wsl (executable-find "wslpath"))
(when running-in-wsl
  (defun wls-copy (text)
    (let ((wls-copy-process (make-process :name "clip.exe"
                                          :buffer nil
                                          :command '("clip.exe")
                                          :connection-type 'pipe)))
      (process-send-string wls-copy-process text)
      (process-send-eof wls-copy-process)))

  (setq interprogram-cut-function 'wls-copy))

(setq initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t)

(defvar file-name-handler-alist-old file-name-handler-alist)

;; Lower threshold to speed up garbage collection
(add-hook 'after-init-hook
          #'(lambda ()
              (setq file-name-handler-alist file-name-handler-alist-old)
              (setq gc-cons-threshold (* 2 1000 1000))
              (setq gc-cons-percentage 0.1))
          t)
;;; Backups
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups")))
      vc-make-backup-files t
      version-control t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t)

;;; PACKAGE LIST
(setq package-install-upgrade-built-in t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ;; ("gnu-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; BOOTSTRAP USE-PACKAGE
(package-initialize)

(eval-when-compile (require 'use-package))
(setq use-package-verbose t
      native-comp-async-report-warnings-errors nil)

;; Install and load `quelpa-use-package'.
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;; Keep custom-set-variables and friends out of my init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; ASYNC
;; Emacs look SIGNIFICANTLY less often which is a good thing.
;; asynchronous bytecode compilation and various other actions makes
(use-package async
  :ensure t
  :after dired
  :init
  (dired-async-mode 1))

(use-package savehist
  :defer 2
  :init
  (savehist-mode t)
  ;; So I can always jump back to wear I left of yesterday
  (add-to-list 'savehist-additional-variables 'global-mark-ring))

(use-package repeat
  :defer 10
  :init
  (repeat-mode +1))

;;; MY STUFF
(use-package custom-variables
  :ensure nil :no-require t :demand t
  :init
  (cl-defmacro let-regex ((bindings (string regex)) &body body)
    "Macro for creating BINDINGS to captured blocks of REGEX found in a STRING.
BINDINGS: A list of different symbols to be bound to a captured section of the regex
STRING: The string the regex is searching through
REGEX: Regex used to match against the string

If no binding is captured section of regex is found for a BINDING an error is signaled
   ;; Example usage
   (let-regex ((h w) (\"hello world\" \"\\(hello\\) \\(world\\)\"))
                (message \"result was %s then %s\" h w))"
    (let ((holder (gensym)))
      `(let ((,holder (with-temp-buffer
                        (insert ,string)
                        (beginning-of-buffer)
                        (search-forward-regexp ,regex nil nil)
                        (let ((i 0))
                          (mapcar (lambda (_a)
                                    (setq i (+ i 1))
                                    (match-string i))
                                  '( ,@bindings))))))
         (let ,(mapcar (lambda (binding)
                         `(,binding (or (pop ,holder)
                                        (error "Failed to find binding for %s"
                                               ',binding))))
                       bindings)
           ,@body))))
  (defvar my/is-termux
    (string-suffix-p
     "Android" (string-trim (shell-command-to-string "uname -a")))
    "Truthy value indicating if Emacs is currently running in termux.")
  (defvar my/is-terminal
    (not window-system)
    "Truthy value indicating if Emacs is currently running in a terminal.")
  (defvar my/my-system
    (equal "gavinok" (getenv "USER"))
    "Non-nil value if this is my system."))

(use-package image-mode
  :bind (:map image-mode-map
              ("&" . my/shell-command-on-file)
              ("<down-mouse-1>" . (lambda ()
                                    (interactive)
                                    (dnd-begin-file-drag (buffer-file-name)))))
  :config
  (defun my/dnd-this-file ()
    "Begin dragging the current file using drag and drop"
    (interactive)
    (dnd-begin-file-drag (buffer-file-name))))

(use-package custom-functions
  :ensure nil :no-require t
  :bind (([remap scroll-up-command] . my/scroll-down)
         ([remap scroll-down-command].  my/scroll-up)
         ("C-M-&"   . my/shell-command-on-file)
         ("C-x O"   . other-other-window)
         ("M-f"     . sim-vi-w)
         ("C-x n a" . my/increment-number-at-point)
         ("C-x n x" . my/decrement-number-at-point)
         ("C-c d"   . my/next-fn)
         :repeat-map my/next-fn-map
         ("d" . my/next-fn)
         :repeat-map change-number-repeat-map
         ("a" . my/increment-number-at-point)
         ("x" . my/decrement-number-at-point))
  :init
  (defun my/next-fn (&optional arg)
    (interactive "P")
    (apply (if arg
               #'text-property-search-backward
             #'text-property-search-forward)
           'face
           (cond
            ((eql major-mode 'haskell-mode) 'haskell-definition-face)
            (t                              'font-lock-function-name-face))
           nil))

  (defun my/change-number-at-point (change increment)
    (search-forward-regexp (rx digit)) ; Go to the closest number
    (let ((number (number-at-point))
          (point (point)))
      (when number
        (progn
          (forward-word)
          (search-backward (number-to-string number))
          (replace-match (number-to-string (funcall change number increment)))
          (goto-char (- point 1))))))

  (defun my/increment-number-at-point (&optional increment)
    "Increment number at point like vim's C-a"
    (interactive "p")
    (my/change-number-at-point '+ (or increment 2)))

  (defun my/decrement-number-at-point (&optional increment)
    "Decrement number at point like vim's C-x"
    (interactive "p")
    (my/change-number-at-point '- (or increment 1)))

  (defun my/scroll-down (_arg)
    "Move cursor down half a screen ARG times."
    (interactive "p")
    (let ((dist (/ (window-height) 2)))
      (forward-line dist)))

  (defun my/scroll-up (_arg)
    "Move cursor up half a screen ARG times."
    (interactive "p")
    (let ((dist (/ (window-height) 2)))
      (forward-line (- dist))))

  (defun my/shell-command-on-file (command)
    "Execute COMMAND asynchronously on the current file."
    (interactive (list (read-shell-command
                        (concat "Async shell command on " (buffer-name) ": "))))
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (async-shell-command (concat command " " filename))))

  (defun eqn-to-tex (eqn-expression)
    "Takes a eqn expression as a string string EQN-EXPRESSION and
returns the equivalent latex version."
    (calc-eval `(,eqn-expression
                 calc-simplify-mode none
                 calc-language eqn)
               'push)
    (calc-eval '(1
                 calc-simplify-mode none
                 calc-language latex)
               'top))

  (defun echo-eqn-to-tex (eqn-expr &optional _arg)
    "Takes an eqn expression eqn-expr and prints a message with the
latex version of it."
    (interactive "sEnter eqn here: ")
    (message (eqn-to-tex eqn-expr)))

  (defun eqn-to-tex-region (start end)
    "Replaces the active region containing a eqn expression and
replaces it with the Latex equivalent."
    (interactive "r")
    (let ((converted-expr (eqn-to-tex (filter-buffer-substring start end))))
      (kill-region start end)
      (insert converted-expr)))

  (defun all-history ()
    "Command for getting command history from basically every source out
     there.

     No more \"Where did I call that again?\" going off in your head"
    (interactive)
    (cl-flet ((file->lines (file-name)
                (split-string (with-temp-buffer
                                (insert-file-contents-literally file-name)
                                (buffer-substring-no-properties (point-min) (point-max)))
                              "\n")))
      (completing-read
       "All History: "
       (append shell-command-history
               compile-history
               (when (boundp 'eshell-history-file-name)
                 (file->lines eshell-history-file-name))
               (file->lines (getenv "HISTFILE"))))))

  (defun gist-from-region (BEG END fname desc &optional private)
    "Collect the current region creating a github gist with the
filename FNAME and description DESC.
If the optional argument PRIVATE is non-nil then the gist will be
made private. Otherwise the gist will be default to public.

Depends on the `gh' commandline tool"
    (interactive (list (mark) (point)
                       (read-string "File Name: ")
                       (read-string "Description: ")
                       current-prefix-arg))
    (let* ((extra-args (unless private '("--public")))
           (command `("gh" "gist" "create"
                      "--filename" ,fname
                      "--desc" ,desc
                      ,@extra-args
                      "-"))
           (proc (make-process :name "Gist Creation"
                               :buffer "*Gist URL*"
                               :command command
                               :sentinel (lambda (process event)
                                           "Listens for process finish and prints the gist's URL"
                                           (unless (process-live-p process )
                                             (if (string-match "finis.*" event)
                                                 (let ((select-enable-clipboard t)
                                                       (url (with-current-buffer (process-buffer process)
                                                              (goto-char (point-max))
                                                              (thing-at-point 'line))))
                                                   (message "Gist for file %s created at %s (copied to clipboard)"
                                                            fname url)
                                                   (kill-new url))
                                               (switch-to-buffer "*Gist URL*")))))))
      (message "Creating Gist")
      (process-send-region proc BEG END)
      (process-send-eof proc)
      (process-send-eof proc)))

  (defun other-other-window (&optional arg)
    (interactive)
    (if arg
        (other-window (- arg))
      (other-window -1)))

  (defun sim-vi-w (&optional arg)
    "Simulate Vi's \"w\" behavior"
    (interactive "P")
    (forward-word arg)
    (search-forward-regexp "[^[:space:]]")
    (forward-char -1)))

;;; Defaults
(use-package undo-fu-session ; Persistant undo history
  :ensure t
  :demand t
  :bind (("C-x u"   . undo-only)
         ("C-/" . undo-only)
         ("C-?" . undo-redo)
         ("C-z"     . undo-only)
         ("C-S-z"   . undo-redo))
  :config (global-undo-fu-session-mode))

(use-package emacs
  :ensure nil
  :demand t
  :bind (("C-c w"   . fixup-whitespace)
         ("C-x C-d" . delete-pair)
         ("M-c"     . capitalize-dwim)
         ("M-u"     . upcase-dwim)
         ("M-l"     . downcase-dwim)
         ("M-z"     . zap-up-to-char)
         ("C-x S"   . shell)
         ("C-x M-t" . transpose-regions)
         ("C-;"     . negative-argument)
         ("C-M-;"   . negative-argument)
         ("C-g"   . my/keyboard-quit-only-if-no-macro)
         )

  :config
  ;; According to the POSIX, a line is defined as "a sequence of zero or
  ;; more non-newline characters followed by a terminating newline".
  (setopt require-final-newline t)
  (setopt kill-region-dwim 'emacs-word)
  (defun my/keyboard-quit-only-if-no-macro ()
    "A workaround to let me accidently hit C-g while recording a macro"
    (interactive)
    (if (or defining-kbd-macro executing-kbd-macro)
        (progn
          (if (region-active-p)
              (deactivate-mark)
            (message "Macro running. Can't quit.")))

      (keyboard-quit)))
  ;; Set the title of the frame to the current file - Emacs
  (setq-default frame-title-format '("%b - Emacs"))

  ;; No delay when deleting pairs
  (setopt delete-pair-blink-delay 0)
  (blink-cursor-mode -1)
  ;; change truncation indicators
  (define-fringe-bitmap 'right-curly-arrow
    [#b10000000 #b10000000 #b01000000
                #b01000000 #b00100000 #b00100000
                #b00010000 #b00010000 #b00001000
                #b00001000 #b00000100 #b00000100])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00000100 #b00000100 #b00001000
                #b00001000 #b00010000 #b00010000
                #b00100000 #b00100000 #b01000000
                #b01000000 #b10000000 #b10000000])

;;;; Defaults
  ;; Handle long lines
  (setopt bidi-paragraph-direction 'left-to-right)
  (setopt bidi-inhibit-bpa t)
  (global-so-long-mode 1)

  (setopt history-length 1000
          use-dialog-box nil
          delete-by-moving-to-trash t
          create-lockfiles nil
          auto-save-default nil
          ring-bell-function 'ignore
          delete-pair-push-mark t)

;;;; UTF-8
  (prefer-coding-system 'utf-8)
;;;; Remove Extra Ui
  (setopt use-short-answers t) ; don't ask to spell out "yes"
  (setopt show-paren-context-when-offscreen 'overlay) ; Emacs 29
  (show-paren-mode 1)              ; Highlight parenthesis

  ;; (if running-in-wsl
  ;;     (setq-default select-enable-primary t) ; use primary as clipboard in emacs
  ;;   (setq select-enable-primary nil))

  (setq-default select-enable-primary t)
  ;; avoid leaving a gap between the frame and the screen
  (setq-default frame-resize-pixelwise t)

  ;; Vim like scrolling
  (setq scroll-step            1
        scroll-conservatively  10000
        next-screen-context-lines 5
        ;; move by logical lines rather than visual lines (better for macros)
        line-move-visual nil)

  ;;TRAMP
  (setq tramp-default-method "ssh"
        shell-file-name "bash")         ; don't use zsh

  ;; For using local lsp's with tramp
  ;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; recentf
  (setopt recentf-make-menu-items 150)
  (setopt recentf-make-saved-items 150)
  (pixel-scroll-precision-mode t))

(use-package time
  :init
  (setopt world-clock-time-format "%a %d %b %I:%M%p %Z")
  (setopt zoneinfo-style-world-list
          '(("Asia/Hawaii" "Hilo")
            ("America/Edmonton" "Calgary")
            ("America/Vancouver" "Vancouver")
            ("America/New_York" "New York")
            ("America/Whitehorse" "Whitehorse")
            ("Europe/London" "London")
            ("Asia/Tokyo" "Tokyo"))))

(use-package font-setup :ensure nil :no-require t
  :demand t
  :when my/my-system
  :init
  ;; Fonts
  ;; The concise one which relies on "implicit fallback values"
  (use-package fontaine
    :ensure t
    :unless my/is-terminal
    :config
    (setopt fontaine-presets
            '((regular
               :default-height 140)
              (small
               :default-height 110)
              (large
               :default-weight semilight
               :default-height 180
               :bold-weight extrabold)
              (extra-large
               :default-weight semilight
               :default-height 210
               :line-spacing 5
               :bold-weight ultrabold)
              (t                        ; our shared fallback properties
               :default-family "PragmataPro Mono Liga")))
    (fontaine-set-preset 'regular))
  ;; Load pragmatapro-lig.el
  ;; (load (locate-user-emacs-file
  ;;        "lisp/pragmatapro-prettify-symbols-v0.829.el"))
  ;; (add-hook 'prog-mode-hook 'prettify-hook)
  ;; (require 'pragmatapro-lig)
  ;; Enable pragmatapro-lig-mode for specific modes
  ;; (set-fontset-font "fontset-default" 'unicode "Symbola" nil 'prepend)
  ;; Use twitter emojis cuz I like them
  (set-fontset-font "fontset-default" 'emoji "Twemoji" nil 't))
(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((templ "https://github.com/vrischmann/tree-sitter-templ")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
          (typst "https://github.com/uben0/tree-sitter-typst")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (zig "https://github.com/tree-sitter-grammars/tree-sitter-zig")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")))
  (setopt treesit-font-lock-level 4))

(use-package unified-marks :ensure nil :no-require t
  :custom
  (global-mark-ring-max 256)
  (set-mark-command-repeat-pop 256)
  :init
  ;; Unify Marks
  (define-advice push-mark (:after (&optional _location _nomsg _activate)
                                   my/push-mark-global)
    "Always push to the global mark when push-mark is called"
    (let ((old (nth global-mark-ring-max global-mark-ring))
          (history-delete-duplicates nil))
      (add-to-history
       'global-mark-ring (copy-marker (mark-marker))
       global-mark-ring-max t)
      (when old
        (set-marker old nil)))))
;;; General Key Bindings
(use-package crux
  :ensure t
  :bind (("C-x w v" . crux-swap-windows)
         ("C-S-o"   . crux-smart-open-line-above)
         ("C-o"     . crux-smart-open-line)
         ("C-x B"   . my/org-scratch)
         :map dired-mode-map
         ("O" . crux-open-with))
  :config
  (defun my/org-scratch ()
    (interactive)
    (let ((initial-major-mode 'org-mode))
      (crux-create-scratch-buffer))))

(use-package simple
  :ensure nil
  :bind (("M-SPC" . cycle-spacing)))
;;; TERMINAL SETTINGS
(when my/is-terminal
  (progn (set-face-background 'default "undefinded")
         (add-to-list 'term-file-aliases
                      '("st-256color" . "xterm-256color"))
         (xterm-mouse-mode t)))

;;; Aligning Text
(use-package align
  :ensure nil
  :defer t
  :bind ("C-x a a" . align-regexp)
  :config
  ;; Align using spaces
  (defun align-regexp-with-spaces (ogfn &rest args)
    (let ((indent-tabs-mode nil))
      (apply ogfn args)))
  (advice-add 'align-regexp :around #'align-regexp-with-spaces))

;;; COMPLETION
(use-package vertico
  :ensure t
  :init
  ;; Enable vertico using the vertico-flat-mode
  ;; (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (use-package orderless
    :ensure t
    :commands (orderless)
    :custom (completion-styles '(orderless flex)))
  (use-package marginalia
    :ensure t
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :config
    (marginalia-mode))
  (vertico-mode t)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1))
;;;; Extra Completion Functions
(use-package consult
  :ensure t
  :after vertico
  :bind (("C-x b"       . consult-buffer)
         ("C-x C-k C-k" . consult-kmacro)
         ("M-y"         . consult-yank-pop)
         ("M-g g"       . consult-goto-line)
         ("M-g M-g"     . consult-goto-line)
         ("M-g f"       . consult-flymake)
         ("M-g i"       . consult-imenu)
         ("M-g I"       . consult-imenu-multi)
         ("M-s l"       . consult-line)
         ("M-s L"       . consult-line-multi)
         ("M-s u"       . consult-focus-lines)
         ("M-s g"       . consult-ripgrep)
         ("M-s M-g"     . consult-ripgrep)
         ("M-s f"       . consult-find)
         ("M-s M-f"     . consult-find)
         ("C-x C-SPC"   . consult-global-mark)
         ("C-x M-:"     . consult-complex-command)
         ("C-c n"       . consult-org-agenda)
         ("M-X"         . consult-mode-command)
         :map minibuffer-local-map
         ("M-r" . consult-history)
         :map Info-mode-map
         ("M-g i" . consult-info)
         :map org-mode-map
         ("M-g i"  . consult-org-heading))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (recentf-mode t))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-j" . consult-dir)
         :map vertico-map
         ("C-x C-j" . consult-dir)))
(use-package consult-recoll
  :ensure nil
  :vc (:url "https://codeberg.org/jao/consult-recoll.git")
  :bind (("M-s r" . consult-recoll))
  :init
  (setq consult-recoll-inline-snippets t)
  :config
  (defconst recollindex-buffer "*RECOLLINDEX*")
  (defun my/kill-recoll-index ()
    (interactive)
    (let ((proc (get-buffer-process (get-buffer recollindex-buffer))))
      (when (process-live-p proc)
        (kill-process proc))))
  (defun my/recoll-index (&optional _args)
    "Start indexing deamon if there is not one running already.
This way our searches are kept up to date"
    (interactive)
    (let ((recollindex-buffer "*RECOLLINDEX*"))
      (unless (process-live-p (get-buffer-process (get-buffer recollindex-buffer)))
        (make-process :name "recollindex"
                      :buffer recollindex-buffer
                      :command '("recollindex" "-m" "-D")))))
  (eval-after-load 'consult-recoll
    (my/recoll-index))


  ;; Keeping this here until the next release
  (defun consult-recoll--search (&optional initial)
    "Perform an asynchronous recoll search via `consult--read'.
If given, use INITIAL as the starting point of the query."
    (consult--read (consult--async-pipeline
                    (consult--process-collection #'consult-recoll--command)
                    (consult--async-map #'consult-recoll--transformer)
                    (consult--async-filter #'identity))
                   :annotate #'consult-recoll--annotation
                   :prompt consult-recoll-prompt
                   :require-match t
                   :lookup #'consult--lookup-member
                   :sort nil
                   :state (and (not consult-recoll-inline-snippets)
                               #'consult-recoll--preview)
                   :group (and consult-recoll-group-by-mime
                               #'consult-recoll--group)
                   :initial initial
                   :history '(:input consult-recoll-history)
                   :category 'recoll-result)))

(use-package embark
  :ensure t
  :bind
  ;; pick some comfortable binding
  (("C-="                     . embark-act)
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-d" . dragon-drop)
   :map embark-defun-map
   ("M-t" . chatgpt-gen-tests-for-region)
   ("M-t" . chatgpt-explain-region)
   :map embark-general-map
   ("M-c" . chatgpt-prompt)
   :map embark-region-map
   ("D"   . dictionary-search)
   ("?"   . chatgpt-explain-region)
   ("M-f" . chatgpt-fix-region)
   ("M-f" . chatgpt-fix-region))
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
                                 (concat "dragon-drop " file)))

  ;; Preview any command with M-.
  (keymap-set minibuffer-local-map "M-." 'my-embark-preview)
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim))))))

;; Helpful for editing consult-grep
(use-package wgrep :ensure t :after embark
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; For uploading files
(use-package 0x0
  :ensure t
  :after embark
  :bind (
         :map embark-file-map
         ("U"    . 0x0-upload-file)
         :map embark-region-map
         ("U"    . 0x0-dwim))
  :commands (0x0-dwim 0x0-upload-file))

(use-package completion-preview
  :demand t
  :hook (prog-mode . completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
              ("C-i" . completion-preview-insert)
              ("M-n" . completion-preview-insert-word)
              ("M-p" . completion-preview-prev-candidate))
  :custom
  (completion-preview-minimum-symbol-length 1))

;;;; Code Completion
(use-package corfu
  :disabled
  :ensure t
  ;; Optional customization
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)) ; Popup completion info

(use-package cape
  :ensure t
  :defer 10
  :bind ("C-c f" . cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defun my/add-shell-completion ()
    (interactive)
    (add-to-list 'completion-at-point-functions 'cape-history)
    (add-to-list 'completion-at-point-functions 'pcomplete-completions-at-point))
  (add-hook 'shell-mode-hook #'my/add-shell-completion nil t)
  :config
  ;; Make capfs composable
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)

  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package tempel
  :ensure t
  :hook ((prog-mode . tempel-setup-capf)
         (text-mode . tempel-setup-capf))
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ([remap keyboard-escape-quit] . tempel-done)
         ("TAB" . tempel-next)
         ("<backtab>" . tempel-previous))
  :config
  (defun tempel-include (elt)
    "Support i as a way to import another template"
    (when (eq (car-safe elt) 'i)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))

  (add-to-list 'tempel-user-elements #'tempel-include)

  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))))

(use-package eglot-tempel
  :ensure t
  :after (:all eglot tempel)
  :config
  (eglot-tempel-mode t))

;;; Themeing
(use-package spaceway-theme
  :ensure nil
  :load-path "lisp/spaceway/"
  :config
  (global-hl-line-mode t)
  (set-frame-parameter nil 'cursor-color "#dc322f")
  (add-to-list 'default-frame-alist '(cursor-color . "#dc322f"))

  (when my/my-system
    (set-frame-parameter nil 'alpha-background 100)
    (add-to-list 'default-frame-alist '(alpha-background . 100)))

  (load-theme 'spaceway t)
  (setenv "SCHEME" "dark"))
(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode)
  :custom
  ;; make the modeline look minimal
  (spacious-padding-subtle-mode-line '( :mode-line-active default
                                        :mode-line-inactive vertical-border)))

;;; WRITING
(use-package writegood-mode
  :ensure t
  :hook ((markdown-mode nroff-mode org-mode
                        mail-mode
                        git-commit-mode)
         . writegood-mode)
  :config
  (writegood-duplicates-turn-off))

(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode global-writeroom-mode)
  :init
  (setq writeroom-width 90))

(use-package jinx
  :unless (eq system-type 'android)
  :demand t
  :ensure t
  :bind ("C-c DEL" . jinx-correct)
  :init
  (global-jinx-mode)
  (add-to-list 'ispell-skip-region-alist '("+begin_src" . "+end_src"))
  (setopt flyspell-use-meta-tab nil))

;;; ORG
(load (locate-user-emacs-file
       "lisp/org-config.el"))

(add-to-list 'load-path
             "/usr/share/emacs/site-lisp/mu4e/")
;;; Email
(load (locate-user-emacs-file
       "lisp/mu4e-config.el"))
;;; Git
(use-package magit
  :bind (("C-x v SPC" . magit-status)
         :map project-prefix-map
         ("m" . project-magit))
  :commands (magit project-magit)
  :config
  (use-package git-commit
    :ensure nil
    :hook ((git-commit-setup . (lambda () (call-interactively 'git-commit-signoff)))))
  (add-to-list 'project-switch-commands
               '(project-magit "Magit" m))
  (defun project-magit  ()
    (interactive)
    (let ((dir (project-root (project-current t))))
      (magit-status dir))))
(use-package orgit-forge :ensure t :after magit)
(use-package forge :ensure t :after magit)
(use-package git-link
  :ensure t
  :bind (("C-c g l" . git-link)))
(use-package magit-todos
  :ensure t
  :after (magit)
  :config
  (magit-todos-mode 1))
;; Lets me store links to a particular git revision
(use-package ol-git-link :after ol)

(use-package ediff
  :after (magit vc)
  :commands (ediff)
  :init
  ;; multiframe just doesn't make sense to me
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook 'winner-undo))
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl
  :ensure t
  :unless my/is-termux
  :defer 5
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-pre-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode)
  :config (diff-hl-flydiff-mode))

;;; EAT AND ESHELL
(use-package eat
  :ensure t
  :custom
  (eat-enable-auto-line-mode t)
  :bind (("C-x E" . eat)
         :map project-prefix-map
         ("t" . eat-project)))

(use-package em-alias
  :ensure nil
  :after eshell
  ;; :config
  ;; (defun my/setup-eshell-aliases ()
  ;;   (eshell/alias "e" "find-file $1")
  ;;   (eshell/alias "ee" "find-file-other-window $1")
  ;;   (eshell/alias "v" "view-file $1")
  ;;   (eshell/alias "o" "crux-open-with $1"))
  ;; (add-hook 'eshell-mode-hook my/setup-eshell-aliases)
  )
(use-package flymake-proselint
  :ensure t
  :after flymake
  :hook ((markdown-mode org-mode text-mode) . flymake-proselint-setup))

(use-package em-term
  :ensure nil
  :after eshell
  :config
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-options '("ghcup" "tui"))
  (add-to-list 'eshell-visual-commands '("htop" "top" "git" "log" "diff"
                                         "show" "less" "nix")))

(use-package eshell
  :commands eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t))

;; More accurate color representation than ansi-color.el
(use-package xterm-color
  :ensure t
  :after esh-mode
  :config
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))

;; Interactive opening of files image preview and more from any repl

;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(use-package project
  :ensure nil
  :demand t
  :bind (("M-s M-s" . project-find-file)
         :map project-prefix-map
         ("m" . project-magit)
         ("d" . project-dired)
         ("M-s" . project-find-file))
  :init
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-dired "Dired")
            (project-magit "Magit")
            (project-compile "Compile")
            (project-find-regexp "Find Regex")
            (eat-project "Terminal"))
          project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  :config
  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;; 1. project.el (project-roots)
  (setq consult-project-function
        (lambda (may-prompt)
          (when-let* ((project (project-current))
                      (project-root (car (project-roots (project-current))))
                      (is-not-home
                       (not (string= "/home/gavinok/" (car (project-roots
                                                            (project-current)))))))
            project-root)))

  ;; Added in emacs 29
  (setopt project-vc-extra-root-markers
          '("*.cabal" "pyproject.toml"
            "spago.dhall" "CMakeList.txt"
            "package.clj" "package.json"
            "Project.toml" ".project"
            "Cargo.toml""mix.exs" "qlfile" ".git"))

  ;; Override project.el backends for specific directories
  ;;   (defvar my/overrided-project '("~/test/"))

  ;;   (defun my/known-project (dir)
  ;;     "Return if the given directory DIR is a known project root."
  ;;     (when (sequencep project--list)
  ;;       (cl-find (expand-file-name dir)
  ;;                (mapcar #'expand-file-name
  ;;                        (mapcar #'car project--list))
  ;;                :test #'equal)))

  ;;   ;; TODO don't run project-current multiple times
  ;;   (defun project-override (dir)
  ;;     "If the directory DIR is a known project root simply use that
  ;; If not try the alternative backends and if one is black listed prompt
  ;; the user to enter an alternative manually."
  ;;     (or (list 'overrided nil (my/known-project dir))
  ;;         (let ((project-find-functions (remove 'project-override project-find-functions)))
  ;;           (when (cl-find (expand-file-name
  ;;                           (project-root (project-current nil dir)))
  ;;                          (mapcar #'expand-file-name
  ;;                                  eglot-single-file-project-blacklist)
  ;;                          :test #'equal)
  ;;             (condition-case ex
  ;;                 (let ((root (read-directory-name "Select Project Root " dir)))
  ;;                   (project--remember-dir root)
  ;;                   (list 'overrided nil root))
  ;;               (error
  ;;                nil))))))

  ;;   (add-to-list 'project-find-functions 'project-override)

  ;;   (cl-defmethod project-root ((project (head overrided)))
  ;;     (nth 2 project))
  )

;; TODO give this and https://github.com/mohkale/compile-multi a try
(use-package projection
  :disabled
  :ensure t
  :after project
  :bind-keymap
  ("C-x P" . projection-map))

;;; COMPILATION
(use-package compile
  :defer t
  :hook ((compilation-filter . ansi-color-compilation-filter))
  ;; Using C-u before recompile acts identical to the M-x compile
  ;; :bind (("C-x C-m" . recompile))
  :config
  (setopt compilation-scroll-output t)
  (setopt compilation-ask-about-save nil)
  (require 'ansi-color)
  ;; Custom compilers
  (defun generic-compiler ()
    (concat "compiler "
            (if buffer-file-name
                (shell-quote-argument buffer-file-name))))

  (defvar custom-compiler-modes
    `((purescript-mode . "spago run")
      (vue-ts-mode    . "npx eslint --fix . && npx vue-tsc --noEmit")))

  (defun get-compiler ()
    (let* ((compiler (assoc-default major-mode
                                    custom-compiler-modes
                                    'eql nil)))
      (cond ((or (file-exists-p "makefile")
                 (file-exists-p "Makefile"))
             "make -k ")
            ((functionp compiler) (funcall compiler))
            ((stringp compiler) compiler)
            (t (funcall #'generic-compiler)))))

  ;; A total hack I realized I could do thanks to M-x compile
  ;; executing `(let ((command (eval compile-command))) ...)'
  (setq-default compile-command '(get-compiler))

  ;; Auto focus compilation buffer
  (add-hook 'compilation-finish-functions 'finish-focus-comp)
  (add-hook 'compilation-start-hook 'finish-focus-comp)

  (defun finish-focus-comp (&optional buf-or-proc arg2)
    (let* ((comp-buf (if (processp buf-or-proc)
                         (process-buffer buf-or-proc)
                       buf-or-proc))
           (window (get-buffer-window comp-buf)))
      (if window
          (select-window window)
        (switch-to-buffer-other-window comp-buf)))))

;;; BUFFER MANAGMENT
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Modify the default ibuffer-formats
  (setopt ibuffer-formats
          '((mark modified read-only " "
                  (name 40 40 :left :elide)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  filename-and-process)))
  (setopt ibuffer-saved-filter-groups
          '(("home"
             ("Windows" (and (mode . exwm-mode)
                             (not (name . "qutebrowser"))))
             ("Qutebrowser" (name . "qutebrowser"))
             ("Shells" (mode . shell-mode))
             ("emacs-config" (or (filename . ".emacs.d")
                                 (filename . "emacs-config")))

             ("Web Dev" (or (mode . html-mode)
                            (mode . css-mode)))
             ("Magit" (name . "*magit"))
             ("Help" (or (name . "*Help*")
                         (name . "*Apropos*")
                         (name . "*info*")))
             ("Browser" (mode . eaf-mode))
             ("Ement" (name . "*Ement *"))
             ("Org" (or (mode . org-mode)
                        (filename . "OrgMode"))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "home"))))

;;; ISEARCH
(use-package isearch
  :ensure nil
  :bind (("C-s"     . isearch-forward)
         ("M-R"     . isearch-query-replace)
         ("C-r"     . isearch-backward)
         ("M-s c"   . toggle-case-fold-search)
         (:map isearch-mode-map
               ("M-w" . isearch-save-and-exit)
               ("M-R" . isearch-query-replace)
               ("M-/" . isearch-complete))
         (:repeat-map isearch-repeat-map
                      ("s" . isearch-repeat-forward)
                      ("r" . isearch-repeat-backward)))
  :custom ((lazy-count-prefix-format nil)
           (lazy-count-suffix-format " [%s of %s]")
           (search-whitespace-regexp ".*?")
           (isearch-lazy-highlight t)
           (isearch-lax-whitespace t)
           (isearch-regexp-lax-whitespace nil))
  :config
  (defun isearch-save-and-exit ()
    "Exit search normally. and save the `search-string' on kill-ring."
    (interactive)
    (isearch-done)
    (isearch-clean-overlays)
    (kill-new isearch-string))

  ;; Place cursor at the start of the match similar to vim's t
  ;; C-g will return the cursor to it's orignal position
  (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
  (defun my-goto-match-beginning ()
    (when (and isearch-forward isearch-other-end
               (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end))))

(use-package ffap
  :ensure nil
  :bind ("C-x f" . ffap)
  :custom (find-file-visit-truename t)
  :init
  ;; Save my spot when I jump to another file
  (advice-add 'ffap :before #'push-mark))

;;; popup window managment
(use-package popper
  :ensure t ; or :straight t
  :bind (("M-`"     . popper-toggle)
         ("M-~"     . popper-cycle)
         ("C-x M-`" . popper-toggle-type))
  :init
  (setopt popper-reference-buffers
          '("\\*Messages\\*"
            "\\*Warnings\\*"
            "\\*xref\\*"
            "\\*Backtrace\\*"
            "*Flymake diagnostics.*"
            "\\*eldoc\\*"
            "\\*rustic-"
            "^*tex"
            "\\*Ement Notifications\\*"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            "\\*Dtache Shell Command\\*"
            "\\*mu4e-update\\*"
            "\\*GDB.*out\\*"
            help-mode))
  (setopt popper-display-control t)
  (popper-mode +1))

(use-package replace-from-region
  :ensure t
  :bind (("C-x R" . query-replace-from-region))
  :init
  (unless (package-installed-p 'replace-from-region)
    (package-vc-install
     `(replace-from-region
       :url "https://github.com/rubikitch/replace-from-region.git"))))
(use-package replace
  :defer
  :bind (:map occur-mode-map
              ("C-x C-q" . occur-edit-mode)))

(use-package multiple-cursors
  :ensure t
  :bind (;; Call with a 0 arg to skip one
         ("C-M-." . mc/mark-next-like-this)
         ("C-M-," . mc/mark-previous-like-this))
  :config
  ;; Use phi-search to replace isearch when using multiple cursors
  (defun toggle-corfu-auto-for-mc (&optional _arg)
    (if multiple-cursors-mode
        (corfu-mode -1)
      (corfu-mode 1)))
  (cl-pushnew 'toggle-corfu-auto-for-mc multiple-cursors-mode-hook))

(use-package phi-search
  :after multiple-cursors
  :ensure t
  :bind (:map mc/keymap
              ("C-s" . phi-search)
              ("C-r" . phi-search-backward)
              ("C-w" . kill-region)
              ("C-w" . kill-region)))
(use-package mouse
  :ensure nil
  :defer 3
  :bind(("<wheel-up>"    .  previous-line)
        ("<wheel-down>"  .  next-line)
        ("<wheel-left>"  .  backward-char)
        ("<wheel-right>" .  forward-char)
        ("<mouse-4>"     .  previous-line)
        ("<mouse-5>"     . next-line)
        ("<mouse-6>"     . backward-char)
        ("<mouse-7>"     . forward-char))
  :init
  (context-menu-mode 1))
(use-package strokes-mode
  :unless (eq system-type 'android)
  :ensure nil
  :bind (("S-<down-mouse-1>" . strokes-do-stroke)
         ("S-<down-mouse-3>" . mouse-appearance-menu))
  :commands (strokes-do-stroke strokes-global-set-stroke)
  :init
  (eval-after-load 'vertico
    (vertico-mouse-mode t))
  (strokes-mode t)
  (setopt focus-follows-mouse t))

(use-package autorevert
  :ensure nil
  :defer 1
  :custom (global-auto-revert-non-file-buffers t)
  :init (global-auto-revert-mode t))

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (dabbrev-upcase-means-case-search t)
  :config
  (setopt hippie-expand-try-functions-list
          '(tempel-expand
            try-expand-all-abbrevs
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-complete-file-name-partially
            try-complete-file-name)))

;;; FOLDING
(use-package outline
  ;; :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              ("C-c u" . outline-up-heading)
              ("C-c j" . outline-forward-same-level)
              ("C-c k" . outline-backward-same-level)
              ("C-x C-a" . outline-show-all)
              :repeat-map outline-navigation-repeat-map
              ("TAB" . outline-cycle)
              ("C-TAB" . outline-cycle)
              ([tab] . outline-cycle)
              ("S-TAB" . outline-cycle-buffer)
              ([backtab] . outline-cycle-buffer)
              ("C-a" . outline-show-all))
  :custom
  (outline-minor-mode-use-buttons 'in-margins)
  (outline-minor-mode-highlight 'append)
  (outline-minor-mode-cycle t))
(use-package hideshow
  :ensure nil
  :bind (:map hs-minor-mode-map
              ("C-<tab>" . hs-toggle-hiding)))
;; Automatic code formatting
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  ;; Setup auto formatting for purescript
  (add-to-list 'apheleia-formatters '(purs-tidy "purs-tidy" "format"))
  (setf (alist-get 'purescript-mode apheleia-mode-alist) '(purs-tidy))
  ;; Setup auto formatting for haskell
  (add-to-list 'apheleia-formatters '(fourmolu "fourmolu"))
  (setf (alist-get 'haskell-mode apheleia-mode-alist) '(fourmolu)))

;;; Languages
(use-package extra-languages
  :ensure nil :no-require t
  :init
  (use-package dockerfile-ts-mode :mode "Dockerfile\\'")
  (use-package yaml-ts-mode :mode "\\.\\(yml\\|yaml\\)\\'"
    :bind (:map yaml-ts-mode-map
                ([remap backward-up-list] . outline-up-heading))
    :config
    (defun yaml-outline-level ()
      "Return the outline level based on the indentation, hardcoded at 2 spaces."
      (require 's)
      (s-count-matches "[ ]\\{2\\}" (match-string 0)))
    (add-hook 'yaml-ts-mode-hook
              (lambda ()
                (outline-minor-mode)
                (setq outline-regexp
                      "^\\([ ]\\{2\\}\\)*\\([-] \\)?\\([\"][^\"]*[\"]\\|[a-zA-Z0-9_-]*\\): *\\([>|]\\|&[a-zA-Z0-9_-]*\\)?$")
                (setq outline-level 'yaml-outline-level)))
    (push '((yaml-ts-mode yaml-mode) "helm_ls" "serve") eglot-server-programs))

  (use-package toml-ts-mode :mode "\\.toml\\'")
  (use-package bash-ts-mode :mode "\\.sh\\'"
    :config
    (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))
  (use-package java-ts-mode
    :mode "\\.java\\'"
    :init
    (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
    (setopt java-ts-mode-indent-offset 2)
    (defun set-java-version ()
      (interactive)
      (let ((version-dir (completing-read "Select Java Version: "
                                          (seq-filter (lambda (dir)
                                                        (string-search "java" dir))
                                                      (directory-files "/usr/lib/jvm" t))
                                          #'file-exists-p
                                          t)))
        (setenv "PATH" (concat version-dir
                               "/bin/:"
                               (getenv "PATH")))
        (setenv "JAVA_HOME" version-dir)
        (setq exec-path (append (list (concat version-dir "/bin/"))
                                exec-path)
              process-environment (append
                                   (list (concat
                                          "PATH="
                                          (cl-reduce (lambda (a b) (concat a ":" b)) (exec-path))))
                                   process-environment))

        ;; Set java version for compiling java
        ;; TODO maybe make this optional
        (setq compilation-environment (list
                                       (concat
                                        "PATH="
                                        (cl-reduce (lambda (a b) (concat a ":" b)) (exec-path)))
                                       (concat "JAVA_HOME=" version-dir)))

        (message "JAVA_HOME is now %s and PATH is now %s"
                 (getenv "JAVA_HOME")
                 (getenv "PATH"))))
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((java-ts-mode java-mode)
                     . ("/home/gavinok/java-language-server/dist/lang_server_linux.sh"))))
    ;; (use-package eglot-java
    ;;   :ensure t
    ;;   :after (:and java eglot))
    )
  (use-package go-ts-mode
    :mode "\\.go\\'"
    :init
    (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
    :config
    (with-eval-after-load 'eglot
      ;; Ensure gopls will use inlayhints (IDK why I needed to do this manually)
      (setq-default eglot-workspace-configuration
                    '(
		      :gopls (
			      :hints (
				      :assignVariableTypes t
				      :compositeLiteralFields t
				      :compositeLiteralTypes t
				      :constantValues t
				      :functionTypeParameters t
				      :parameterNames t
				      :rangeVariableTypes t
				      ))
		      :haskell(
			       :maxCompletions 40
			       :checkProject t
			       :formattingProvider "none")))
      ;; Ensure inlay hints going for eglot
      ;; (add-hook 'go-ts-mode-hook 'eglot-inlay-hints-mode)
      ))
  (use-package python
    :mode "\\.py\\'"
    :hook ((python-ts-mode . prettify-symbols-mode))
    :init
    ;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    ;; For some reason python3 is not working with readline native completion
    (setopt python-shell-completion-native-enable nil)
    :config
    (with-eval-after-load 'compile
      (add-to-list 'compilation-error-regexp-alist 'pyright)
      (add-to-list 'compilation-error-regexp-alist-alist
                   '(pyright "^\\ \\ \\([a-zA-Z0-9/\\._-]+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3)))
    (setq python-shell-enable-font-lock nil)
    (use-package pyvenv
      :ensure t
      :commands (pyvenv-activate)
      :config
      (pyvenv-mode t))
    (with-eval-after-load 'dape
      (setopt dape-buffer-window-arrangement 'gud)
      (push
       '(debugpy-attach-port-remote
         modes (python-mode python-ts-mode)
         port (lambda () (read-number "Port: "))
         :request "attach"
         :type "python"
         :pathMappings (lambda ()
                         (let* ((default (list (if-let ((project (project-current)))
                                                   (project-root project)
                                                 default-directory)))
                                (local (completing-read "Local <Path/To/Project>: " default))
                                (remote (completing-read "Remote <Path/To/Project>: "
                                                         ;; Path just we often use at work
                                                         '("/app"))))
                           (vector (list :localRoot local :remoteRoot remote))))
         :justMyCode nil
         :showReturnValue t)
       dape-configs)))
  ;; Haskell
  (use-package haskell-mode :ensure t :mode "\\.hs\\'"
    ;; NOTE: that if you use eglot to ensure that the project.el root
    ;; points at the Cobal file

    ;; lets you use C-c C-l
    :ensure t
    :hook ((haskell-mode . interactive-haskell-mode)
           (haskell-mode . turn-on-haskell-doc-mode)
           (haskell-mode . haskell-indent-mode)
           (haskell-mode . haskell-setup-outline-mode))
    :bind (
           :map haskell-mode-map
           ("M-n" . haskell-goto-next-error)
           ("M-p" . haskell-goto-prev-error))
    :config
    (defun haskell-setup-outline-mode ()
      (make-local-variable 'outline-regexp)
      (setq outline-regexp "\\`\\|\\s-+\\S-"))
    )

  (use-package purescript-mode :ensure t :mode "\\.purs\\'"
    :hook ((purescript-mode . purescript-indent-mode)
           (purescript-mode . turn-on-purescript-unicode-input-method))
    :config
    ;; TODO add compilation-error-regexp-alist for purs
    (use-package psci
      :ensure t
      :bind (:map purescript-mode-map
                  ("C-c C-z" . purescript-switch-to-psci))
      :hook ((purescript-mode . inferior-psci-mode)
             (psci-mode . compilation-shell-minor-mode))
      :init
      (defun run-purescript ()
        (interactive)
        (call-interactively 'psci))
      (defun purescript-switch-to-psci ()
        (interactive)
        (switch-to-buffer-other-window "*psci*"))
      (add-to-list 'compilation-error-regexp-alist 'purescript)
      (add-to-list 'compilation-error-regexp-alist-alist
                   '(purescript "^at \\([a-zA-Z0-9/\\._-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))))

  (use-package rust-ts-mode :ensure t :mode "\\.rs\\'"
    :init
    ;; scratchpad for rust
    (setq lsp-rust-clippy-preference "on")
    (use-package rust-playground
      :commands (rust-playground)
      :ensure t))
  (use-package racket-mode :ensure t :mode "\\.rkt\\'"
    :config
    (require 'racket-xp)
    (add-hook 'racket-mode-hook #'racket-xp-mode)
    ;; (add-hook 'racket-mode-hook #'prettify-symbols-mode)
    (defun setup-racket-eldoc ()
      (eldoc-mode +1)
      (setq eldoc-documentation-function #'racket-xp-eldoc-function))
    (add-hook 'racket-mode-hook #'setup-racket-eldoc))

  (use-package clojure-mode :ensure t :mode "\\.clj\\'")

  (use-package vue-ts-mode
    :mode "\\.vue\\'"
    :hook ((vue-ts-mode . sgml-electric-tag-pair-mode)
           (vue-ts-mode . my/eglot-server-setup-paths)
           (vue-ts-mode . eglot-ensure))
    :bind (:map vue-ts-mode-map
                ("C-c C-t" . vue-ts-mode-element-transpose)
                ("C-c C-w" . vue-ts-mode-attributes-toggle-wrap)
                ("C-c C-o" . vue-ts-mode-element-match)
                ("C-c C-f" . sgml-skip-tag-forward)
                ("C-c C-n" . sgml-skip-tag-forward)
                ("C-c C-b" . sgml-skip-tag-backward)
                ("C-c C-p" . sgml-skip-tag-backward)
                :repeat-map sgml-skip-tag
                ("f"   .  sgml-skip-tag-forward)
                ("C-f" .  sgml-skip-tag-forward)
                ("b"   .  sgml-skip-tag-backward)
                ("C-b" .  sgml-skip-tag-backward))
    :init
    (unless (package-installed-p 'vue-ts-mode)
      (package-vc-install "https://github.com/theschmocker/vue-ts-mode"))
    (with-eval-after-load 'treesit
      (push '(vue "https://github.com/ikatyang/tree-sitter-vue")
            treesit-language-source-alist))
    :config
    (setopt vue-ts-mode-indent-offset 2)
    (setq-default vue-ts-mode-indent-offset 2)
    ;; from https://github.com/joaotavora/eglot/discussions/1184
    (with-eval-after-load 'eglot
      (defun vue-eglot-init-options ()
        ;; installed with (my/eglot-server--npm-dependancy-install "typescript")
        (let ((tsdk-path (expand-file-name
                          "lib"
                          (string-trim-right
                           (shell-command-to-string (concat "npm list  --prefix "
                                                            (my/eglot-server--npm-package-path "typescript")
                                                            " --global --parseable typescript | head -n1"))))))
          `(:typescript (:tsdk ,tsdk-path
			       :languageFeatures (:completion
                                                  (:defaultTagNameCase "both"
								       :defaultAttrNameCase "kebabCase"
								       :getDocumentNameCasesRequest nil
								       :getDocumentSelectionRequest nil)
                                                  :diagnostics
                                                  (:getDocumentVersionRequest nil))
			       :documentFeatures (:documentFormatting
                                                  (:defaultPrintWidth 100
								      :getDocumentPrintWidthRequest nil)
                                                  :documentSymbol t
                                                  :documentColor t)))))
      (push `(vue-ts-mode . ("vue-language-server" "--stdio"
                             :initializationOptions ,(vue-eglot-init-options)))
            eglot-server-programs))))
(use-package devdocs
  :ensure t
  :bind ("M-s d" . devdocs-lookup))
(use-package avy
  :ensure t
  :bind ("M-o" . avy-goto-char-timer)
  :custom
  (avy-all-windows 'all-frames)
  (avy-timeout-seconds 0.3))
;;;; WEB
(use-package nvm
  :ensure nil
  :when (getenv "NVM_DIR")
  :commands (my/nvm-use)
  :quelpa (nvm :fetcher github :repo "rejeep/nvm.el")
  :commands (nvm-use nvm-use-for-buffer my/nvm-use)
  :config
  (setq nvm-dir "/home/gavinok/.config/nvm")
  (defun my/nvm-use () (interactive)
         (setenv "NVM_DIR" "~/.config/nvm")
         (nvm-use
          (completing-read "Enter Node Version"
                           (directory-files
                            (concat (or (getenv "NVM_DIR")
                                        "~/.nvm")
                                    "/versions/node")
                            nil "v[0-9.]+")))))

(defun disable-tabs ()
  "It's simple just disable tabs in the file."
  (indent-tabs-mode -1))

(add-hook 'web-mode-hook 'disable-tabs)
(add-hook 'js-ts-mode-hook 'disable-tabs)
(add-hook 'vue-ts-mode-hook 'disable-tabs)
(add-hook 'tsx-ts-mode-hook 'disable-tabs)
(add-hook 'typescript-ts-mode 'disable-tabs)
(use-package typescript-ts-mode
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :hook (tsx-ts-mode . eglot-ensure)
  :config
  ;; Inlay add typehints for typescript
  (with-eval-after-load 'eglot
    (add-to-list  'eglot-server-programs '(((js-mode :language-id "javascript")
                                            (js-ts-mode :language-id "javascript")
                                            (tsx-ts-mode :language-id "typescriptreact")
                                            (typescript-ts-mode :language-id "typescript")
                                            (typescript-mode :language-id "typescript"))
                                           "typescript-language-server" "--stdio"))
    ;; (add-to-list  'eglot-server-programs
    ;;               `((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
    ;;                 .
    ;;                 ("typescript-language-server" "--stdio"
    ;;                  :initializationOptions
    ;;                  (:preferences
    ;;     	      (:includeInlayParameterNameHints "all"
    ;;     					       :includeInlayParameterNameHintsWhenArgumentMatchesName t
    ;;     					       :includeInlayFunctionParameterTypeHints t
    ;;     					       :includeInlayVariableTypeHints t
    ;;     					       :includeInlayVariableTypeHintsWhenTypeMatchesName t
    ;;     					       :includeInlayPropertyDeclarationTypeHints t
    ;;     					       :includeInlayFunctionLikeReturnTypeHints t
    ;;     					       :includeInlayEnumMemberValueHints t)))))
    ))
(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :hook (tsx-ts-mode . eglot-ensure))
(use-package dumb-jump
  :ensure t
  :after web-mode)

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . HTML-mode))
  :hook ((web-mode . eglot-ensure))
  :bind (
         :map web-mode-map
         ("C-M-i" . completion-at-point)
         ("C-M-u" . web-mode-element-parent)
         ("C-M-d" . web-mode-element-child))
  :custom
  ;; TODO
  ;; setup [[https://raw.githubusercontent.com/manateelazycat/lsp-bridge/master/core/tailwind_css_keyword.txt]]
  ;; as a dictionary completion source
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-auto-close-style 1)
  (web-mode-enable-auto-closing nil)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-opening nil)
  (css-indent-offset 2)
  (js-indent-level 2)
  :init
  (define-derived-mode HTML-mode web-mode "HTML")
  (defun HTML-mode-dumb-jump-setup ()
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t))
  (add-hook 'HTML-mode-hook #'HTML-mode-dumb-jump-setup)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((HTML-mode :language-id "html")
                   . ("vscode-html-language-server" "--stdio")))
    )

  ;; workaround to get vscode-html-language-server to provide proper diagnostics
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-auto-close-style 1
        web-mode-enable-auto-indentation nil
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-pairing nil
        web-mode-enable-auto-opening nil))

(use-package impatient-mode :ensure t
  :after web-mode
  :custom (imp-default-user-filters '((html-mode . nil)
				      (web-mode  . nil)
				      (HTML-mode . nil))))
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode       . infer-indentation-style)
         (prog-mode       . (lambda () (setq-local show-trailing-whitespace t)))
         (emacs-lisp-mode . disable-tabs)
         (emacs-lisp-mode . (lambda ()
			      (add-hook 'local-write-file-hooks 'check-parens)))
         (lisp-mode       . disable-tabs)
         ;; Make all scripts executable. Ya this might be sketch but I don't
         (after-save      . executable-make-buffer-file-executable-if-script-p))
  :bind (:map emacs-lisp-mode-map
	      ("C-c RET" . emacs-lisp-macroexpand)
	      ("C-c C-k" . eval-buffer))
  :init
  ;; Don't prompt for a reference
  (setq xref-prompt-for-identifier nil)
  (global-prettify-symbols-mode +1)

  ;; Smart Indentation
  (defun infer-indentation-style ()
    "if our source file uses tabs, we use tabs, if spaces spaces, and if
     neither, we use the current indent-tabs-mode"
    (let ((space-count (how-many "^  " (point-min) (point-max)))
          (tab-count (how-many "^\t" (point-min) (point-max))))
      (if (> space-count tab-count) (setq indent-tabs-mode nil))
      (if (> tab-count space-count) (setq indent-tabs-mode t))))
  (setopt electric-indent-functions
          '((lambda (inserted-char)
              "Treat non hanging indents in python and haskell as the end of an expression."
              (when (member major-mode `(python-ts-mode haskell-mode))
                ;; Do not auto-indent after inserting any empty line
                (when (save-excursion
                        (previous-line)
                        (beginning-of-line)
                        (and (looking-at "^\s*$")
                             ;; Don't leave that whitespace behind
                             (fixup-whitespace)))
                  'no-indent)))))
  )

(use-package emmet-mode
  :ensure t
  :hook ((js-jsx-mode typescript-mode tsx-ts-mode) emmet-jsx-major-modes)
  :bind
  ("C-j" . emmet-expand-line)
  (:map emmet-mode-keymap
        ("TAB" . emmet-next-edit-point)
        ("<backtab>" . emmet-prev-edit-point))
  :config
  (defun my/emmet-expand-capf ()
    (let ((bounds (bounds-of-thing-at-point 'symbol))
          (tap (thing-at-point 'symbol)))
      (list (car bounds) (cdr bounds)
            ;; Just return the symbol at point to so completion will be possible
            ;; TODO Determine if there is a less hacky option
            (lambda (string pred action) (list (thing-at-point 'symbol)))
            ;; Annotate with what emmet expands to
            ;; TODO find a way for this to show since right now
            ;; corfu doesn't display this on a single completion
            :annotation-function (lambda (str) (emmet-transform str))
            ;; Don't try to complete with emmet if there is no possible
            ;; expansion
            :predicate (not (string= (emmet-transform tap)
                                     tap))
            ;; Expand Emmet Template On Match
            :exit-function (lambda (str status)
                             (when (eql status 'finished)
			       (emmet-expand-line nil)))
            ;; Allow for other completions to follow
            :exlcusive 'no)))

  (defun emmet-setup-capf ()
    (setq-local completion-at-point-functions
                (add-to-list 'completion-at-point-functions
                             'my/emmet-expand-capf
                             t)))
  (add-hook 'emmet-mode-hook 'emmet-setup-capf))

;;;; Lisp
(use-package sly
  :ensure t
  :commands (sly sly-connect)
  :init
  (setq sly-symbol-completion-mode nil
        sly-default-lisp 'roswell
        ros-config (locate-user-emacs-file
                    "ros-conf.lisp")
        sly-lisp-implementations
        `((sbcl ("sbcl") :coding-system utf-8-unix)
          (abcl ("abcl") :coding-system utf-8-unix)
          (ecl ("ecl") :coding-system utf-8-unix)
          (roswell ("ros" "-Q" "-l" ,ros-config "run"))
          (qlot ("qlot" "exec" "ros" "-l" ,ros-config "run" "-S" ".")
                :coding-system utf-8-unix))))

;;;; Setup Folding For Programming
(use-package puni
  :ensure t
  :hook (((calc-mode term-mode vterm-mode) . puni-disable-puni-mode)
         (prog-mode . puni-mode)
         (puni-mode  . electric-pair-local-mode))
  :bind (("C-c s" . puni-mode)
         :map puni-mode-map
         ("C-c DEL"                 . jinx-correct)
         ([remap forward-sentence]  . puni-end-of-sexp)
         ([remap backward-sentence] . puni-beginning-of-sexp)
         ([remap forward-sexp]      . puni-forward-sexp-or-up-list)
         ([remap backward-sexp]     . puni-backward-sexp-or-up-list)
         ([remap kill-line]         . puni-kill-line)
         ([remap mark-paragraph]    . puni-expand-region)
         ([remap kill-sexp]         . puni-kill-thing-at-point)
         ("M-k"                     . kill-sexp)
         ;; Remove outer pairs
         ("M-O"                     . puni-splice)
         ("C-)"                     . puni-slurp-forward)
         ("C-("                     . puni-slurp-backward)
         ("C-}"                     . puni-barf-forward)
         ("C-{"                     . puni-barf-backward)
         ("M-("                     . puni-wrap-round)
         ("M-C"                     . puni-clone-thing-at-point)
         ("C-M-t"                   . puni-transpose)
         ("C-M-?"                   . puni-convolute)
         ("C-M-z"                   . puni-squeeze)
         ("M-<backspace>"           . backward-kill-word)
         ("C-w" . kill-region))
  :config
  (setopt puni-blink-region-face 'show-paren-match)
  (puni-global-mode t)

  (defun puni-kill-thing-at-point (&optional arg)
    "Kill the next puni based thing at point"
    (interactive)
    (unless buffer-read-only
      (puni-expand-region)
      (kill-region (region-beginning) (region-end))))

  (defun puni-clone-thing-at-point (&optional arg)
    "Clone the next puni based thing at point"
    (interactive)
    (save-excursion
      (puni-expand-region)
      (kill-ring-save (region-beginning) (region-end)))
    (yank)
    (default-indent-new-line)))

(use-package expreg
  :ensure t
  :bind (("M-h" . expreg-expand)))

;; Getting added in emacs 30 https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67687
;; (load (locate-user-emacs-file
;;        "lisp/etags-regen.el"))
(use-package etags-regen
  :when (executable-find "ctags.emacs")
  :custom (etags-regen-tags-file #'project-tag-file)
  :commands etags-regen-mode
  :bind (("C-c t" . complete-tag)
         ("C-c M-." . my/goto-etags))
  :init
  (setopt etags-program-name "ctags.emacs")
  (defun project-tag-file (_)
    (concat "/tmp/"
            (replace-regexp-in-string "\\/" "-" (project-name (project-current)))
            "-TAGS"))
  (defun my/goto-etags ()
    (interactive)
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (call-interactively 'xref-find-definitions)))
  (defvar etags-regen-mode-map (make-sparse-keymap))
  (add-to-list 'minor-mode-map-alist (cons 'etags-regen-mode etags-regen-mode-map)))

(use-package flymake
  :defer 10
  :bind (("M-g d"   . flymake-show-buffer-diagnostics)
         ("M-g M-d" . flymake-show-project-diagnostics)
         ("M-g M-n" . scan-buf-next-region)
         ("M-g M-p" . scan-buf-previous-region)
         :repeat-map flymake-repeatmap
         ("p" . scan-buf-previous-region)
         ("n" . scan-buf-next-region)
         ("M-p" . scan-buf-previous-region)
         ("M-n" . scan-buf-next-region)
         :map flymake-diagnostics-buffer-mode-map
         ("?" . flymake-show-diagnostic-here)
         :map flymake-project-diagnostics-mode-map
         ("?" . flymake-show-diagnostic-here))
  :hook (prog-mode . flymake-mode)
  :config
  (defun flymake-show-diagnostic-here (pos &optional other-window)
    "Show the full diagnostic of this error.
Used to see multiline flymake errors"
    (interactive (list (point) t))
    (let* ((id (or (tabulated-list-get-id pos)
                   (user-error "Nothing at point")))
           (text (flymake-diagnostic-text (plist-get id :diagnostic))))
      (message text)))
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

;; Use flycheck backends with flymake
(use-package flymake-flycheck
  :ensure t
  :after flymake
  :init
  (setopt flycheck-disabled-checkers '(python-mypy haskell-ghc haskell-hlint))
  :config
  (add-hook 'flymake-mode-hook 'flymake-flycheck-auto))

(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items nil))

(use-package eldoc
  :defer 10
  :init
  ;; (setopt eldoc-echo-area-display-truncation-message t)
  ;; (setopt eldoc-echo-area-use-multiline-p nil)
  ;; Make sure Eldoc will show us all of the feedback at point.
  ;; no more clobbering
  (setopt eldoc-documentation-strategy #'eldoc-documentation-compose)
  (global-eldoc-mode t))

(use-package paren-face
  :ensure t
  :hook ((prog-mode cider-repl-mode eshell-mode
                    fennel-repl-mode geiser-repl-mode inferior-emacs-lisp-mode
                    inferior-lisp-mode inferior-scheme-mode lisp-interaction-mode
                    racket-repl-mode scheme-interaction-mode sly-mrepl-mode)
         . paren-face-mode))

;;; EXTRA UI
;; (use-package hl-todo :ensure t :hook prog-mode)

(use-package pulse
  ;; Highlight cursor postion after movement
  :unless my/is-terminal
  :defer t
  :init (defun pulse-line (&rest _)
          (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(other-window
                     windmove-do-window-select
                     mouse-set-point
                     mouse-select-window))
    (advice-add command :after #'pulse-line)))

(use-package separedit
  :ensure t
  ;; Key binding for modes you want edit
  ;; or simply bind ?global-map? for all.
  :bind (
         :map prog-mode-map
         ("C-c '" . separedit)
         :map minibuffer-local-map
         ("C-c '" . separedit)
         :map help-mode-map
         ("C-c '" . separedit))
  :init
  ;; Default major-mode for edit buffer
  ;; can also be other mode e.g. ?org-mode?.
  (setq separedit-default-mode 'markdown-mode)

  ;; Feature options
  ;; (setq separedit-preserve-string-indentation t)
  ;; (setq separedit-continue-fill-column t)
  ;; (setq separedit-write-file-when-execute-save t)
  ;; (setq separedit-remove-trailing-spaces-in-comment t)
  )
;;;; Display hex colors in emacs
(use-package rainbow-mode :ensure t :commands (rainbow-mode))

;;; DIRED
(use-package dired
  :ensure nil
  :commands (dired)
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
	      ("-" . dired-up-directory))
  :init
  ;; let me drag files into other programs
  (setq dired-mouse-drag-files t)
  (setq dired-bind-jump nil)
  :config
  (setq dired-listing-switches "-aghoA --group-directories-first")
  ;; Hide . and .. in dired
  (setq dired-omit-files
        (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))
;;;;; xdg-open integration
  (require 'dired-x)
  ;; prevent opening extra dired buffers
  ;; emacs 28
  (setq dired-kill-when-opening-new-dired-buffer t))

;;; PASS
(use-package password-store
  :ensure t
  :commands (password-store-copy
             password-store-insert
             password-store-generate
             password-store-get))

;; Authenticate with auth-source-pass
;; (use-package auth-source-pass
;;   :after password-store
;;   :config
;;   (auth-source-pass-enable))

;;; MODELINE
(load (locate-user-emacs-file
       "lisp/modeline.el"))

(use-package tab-bar
  :commands tab-bar-mode
  :bind (:map tab-prefix-map
	      ("P" . my/rename-tab-to-project))
  :config
  (defun my/rename-tab-to-project ()
    (interactive)
    (tab-bar-rename-tab (format "P:%s"(project-name (project-current)))))
  (setopt tab-line-tabs-buffer-group-function 'tab-line-tabs-buffer-group-by-project)
  (setopt tab-bar-show t)
  ;; (setopt tab-bar-auto-width-min '(15 2))
  (defun tab-bar-tab-name-format-comfortable (tab i)
    "Add spacing to tab bar mode"
    (propertize (concat " " (truncate-string-to-width
                             (tab-bar-tab-name-format-default tab i)
                             tab-bar-tab-name-truncated-max
                             nil nil t)
                        " ")
                'face (funcall tab-bar-tab-face-function tab)))
  (defun my/fake-modeline ()
    `((my-tab-modeline
       menu-item
       ,(format-mode-line
         `(,@my/mode-line-left-side
           " "
           ,(when (fboundp 'eglot-managed-p)
	      (if (eglot-managed-p)
                  (propertize (or "∈")
			      'face 'success)
                ""))
           mode-line-process
           (which-function-mode ("" which-func-format ""))
           (vc-mode vc-mode)))
       ignore)))
  (setq tab-bar-format `(tab-bar-separator
                         tab-bar-format-menu-bar
                         tab-bar-format-tabs
                         tab-bar-separator
                         tab-bar-format-add-tab
                         tab-bar-format-align-right
                         ;; my/fake-modeline
                         tab-bar-separator
                         tab-bar-separator
                         tab-bar-org-clock
                         ;; tab-bar-format-global
                         ))

  ;; Use tab bar for displaying the currently clocked in task
  (add-hook 'org-clock-in-hook 'tab-bar-mode)
  (defun tab-bar-org-clock ()
    `((my-tab-modeline
       menu-item
       ,(format-mode-line
         `(:eval (when (org-clock-is-active)
                   (propertize org-mode-line-string
                               'face 'org-agenda-clocking
                               'local-map org-clock-mode-line-map
                               'mouse-face 'mode-line-highlight))))
       tab-bar-my-tab-modeline
       :help "Menu bar")))
  ;; Make sure that the custom sections of the modeline are actually rendered
  ;; (setq-default mode-line-format nil)
  (setopt tab-bar-close-button-show nil
          tab-bar-tab-name-format-function #'tab-bar-tab-name-format-comfortable)

  (tab-bar-mode -1))
;;; Server Setup
(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))
(use-package desktop
  :custom
  (desktop-restore-frames nil)
  (desktop-modes-not-to-save '(tags-table-mode exwm-mode)))
;;; Better PDFs
;;https://github.com/politza/pdf-tools
;; annotate pdfs with c-c c-a
;; hl with c-c c-a h
;; for help M-x pdf-tools-help RET
;; (load (locate-user-emacs-file
;;        "lisp/exwm-config.el"))
(use-package pdf-tools
  :ensure t :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelabels)
  (setq-default pdf-view-display-size 'fit-page))

(use-package keycast
  :ensure t
  :commands (keycast-mode-line-mode))

;;; Winner Mode
(use-package winner
  :ensure nil
  :bind (("s-/" . winner-undo)
         ("s-?" . winner-redo))
  :config
  :init (winner-mode 1)) ; Window Managment Undo

(use-package sqlite-mode-extras
  :ensure t
  :hook ((sqlite-mode . sqlite-extras-minor-mode)))

;; install Ement.
(use-package ement
  :ensure t
  :when my/my-system
  :commands (my/ement-connect)
  :init
  (setopt ement-room-send-org-filter 'ement-room-send-org-filter)
  (setopt ement-room-message-format-spec "%B%r%R%t")
  (defun my/ement-connect ()
    (interactive)
    (ement-connect :user-id "@gavinok:matrix.org"
                   :password (password-store-get "riot.im/gavinok"))))

(setq pixel-scroll-precision-interpolate-page t)

(keymap-global-set "C-v" #'my/scroll-down)
(keymap-global-set "M-v" #'my/scroll-up)

(use-package docker
  :ensure t
  :bind (("C-x d" . docker)))

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")
(setenv "READER" "emacsclient")
;;; Stuff To Ignore
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'list-timers 'disabled nil)

;;; IDE
(use-package eglot
  :bind (:map eglot-diagnostics-map
	      ("M-RET" . eglot-code-actions))
  :commands (eglot eglot-ensure)
  :config
  (setopt eglot-confirm-server-edits '((eglot-rename . nil)
				       (t . maybe-summary)))

  ;; Supposedly improves performance
  (setq read-process-output-max (* 4 1024 1024))
  ;; Definitely slow in larger projects
  (setopt eglot-ignored-server-capabilites '(:documentHighlightProvider))

  ;; My easy way to install eglot servers
  (load (locate-user-emacs-file
         "lisp/eglot-lsp-installer.el"))

  ;; Ensures the paths used for eglot-lsp-installer are configured
  ;; before starting a language server
  (define-advice eglot--connect (:before (&rest _) my/advice-eglot-server-setup-paths)
    (my/eglot-server-setup-paths))

  (add-to-list 'eglot-server-programs
	       `((typst-mode typst-ts-mode) . ("typst-lsp")))
  (add-to-list 'eglot-server-programs
               `((scss-mode)
                 . ("vscode-css-language-server" "--stdio")))

  ;; Setup eldoc the way I like it for emacs
  (defun my/setup-eldoc-for-eglot ()
    "Make sure Eldoc will show us all of the feedback at point."
    (setq-local eldoc-documentation-strategy
                #'eldoc-documentation-compose))
  (add-hook 'eglot-managed-mode-hook #'my/setup-eldoc-for-eglot)

  ;; Make eglot and flymake coexist see
  ;; [[https://github.com/mohkale/flymake-collection/issues/20]]
  ;; for more details
  (add-to-list 'eglot-stay-out-of 'flymake)
  (defun my/eglot+flymake ()
    (add-to-list 'flymake-diagnostic-functions 'eglot-flymake-backend))
  (add-hook 'eglot-managed-mode-hook #'my/eglot+flymake))

(use-package eglot-single-file-mode :no-require t
  :after eglot
  :config
  (defgroup eglot-single-file nil "eglot-single-file"
    :group 'eglot)

  (defcustom eglot-single-file-project-blacklist '("~/")
    "A list of directories that eglot is not allowed to set as the
current workspace."
    :type '(repeat string)
    :group 'eglot-single-file)

  ;; A hacky way to make every file opened in my HOME directory act as
  ;; a single file project see https://github.com/joaotavora/eglot/discussions/1086
  (cl-defmethod eglot-workspace-folders :around (server)
    "Make sure we don't treat the HOME directory as the workspace folder"
    (if (and (project-current)
	     (not (cl-find (expand-file-name
			    (project-root (eglot--project server)))
                           (mapcar #'expand-file-name
                                   eglot-single-file-project-blacklist)
                           :test #'equal)))
        (cl-call-next-method)
      (message "eglot: Single File Mode")
      nil))

  (cl-defmethod eglot-register-capability :around
    (_server (_method (eql workspace/didChangeWatchedFiles)) _id &key _watchers)
    "A workaround to avoid monitoring all files in the current
directory when working with a single file project."
    (if (project-current)
        (cl-call-next-method)
      (message "eglot: Single File Mode")
      nil)))

(use-package consult-eglot
  :ensure t
  :after eglot
  :bind ("M-s s" . consult-eglot-symbols))

(use-package lsp-mode
  :no-require t
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-completion-provider :none))
(use-package eglot-booster
  :after (:any eglot lsp-mode)
  :when (executable-find "cargo")
  :init
  (unless (package-installed-p 'eglot-booster)
    (package-vc-install
     '(eglot-booster :url "https://github.com/jdtsmith/eglot-booster")))
  :config
  ;; tricks for improving performance
  (setopt eglot-events-buffer-config '(:size nil :format full)
          eglot-autoshutdown t
          jsonrpc-event-hook ;nil
          (list #'jsonrpc--log-event)
          )

  ;; Install and setup lsp booster
  (let ((emacs-lsp-booster-repo "https://github.com/blahgeek/emacs-lsp-booster")
        (emacs-lsp-booster-dir (file-name-concat
                                (expand-file-name user-emacs-directory)
                                "emacs-lsp-booster")))
    (if (file-directory-p emacs-lsp-booster-dir)
        (push (file-name-concat emacs-lsp-booster-dir
                                "target/release/")
	      exec-path)
      (progn
        (require 'vc)
        (vc-clone emacs-lsp-booster-repo 'Git emacs-lsp-booster-dir)
        (let ((default-directory emacs-lsp-booster-dir))
          (message "Building emacs-lsp-booster")
          (make-process
           :name "Building emacs-lsp-booster"
           :buffer "*EMACS-LSP-BOOSTER-BUILD*"
           :command (list "cargo" "build" "--release")
           :sentinel
           (lambda (process event)
             ;; TODO switch to using `process-status' instead of `event'
             (with-current-buffer (process-buffer process)
	       (ansi-color-apply-on-region (point-min) (point-max)))
             (pcase event
	       ("finished\n"
                (message "Installed emacs-lsp-booster")
                (push (file-name-concat emacs-lsp-booster-dir "target/release/")
		      exec-path))

	       ((or "open from host-name\n" "open\n" "run\n"))
	       (_ (error "Unhandled case %s for event from Building emacs-lsp-booster" event)))
             ))))))
  (eglot-booster-mode))

(use-package eglot-codelens
  :ensure nil
  :vc (:url "https://github.com/Gavinok/eglot-codelens")
  :after eglot)

(use-package dape
  :commands (dape)
  ;; Currently only on github
  :init
  (unless (package-installed-p 'dape)
    (package-vc-install "https://github.com/svaante/dape"))
  :config
  ;; Add inline variable hints, this feature is highly experimental
  (setopt dape-inline-variables t))

;; (load (locate-user-emacs-file
;;        "lisp/exwm-config.el"))

;; describe this code
;; Random Other Stuff
(use-package devil
  :when (eq system-type 'android)
  :demand t
  :ensure t
  :bind ("<xf86back>" . keyboard-escape-quit)
  :config
  (global-devil-mode 1)
  (setq overriding-text-conversion-style nil))

(use-package elfeed
  :ensure t
  :commands (elfeed)
  :config
  ;; Somewhere in your .emacs file
  (setopt newsticker-url-list
          '(("Gnu World Order" "http://gnuworldorder.info/opus.xml" nil 86400)
            ("hopwag" "https://feed.podbean.com/hopwag/feed.xml" nil 86400)
            ("lukesmith" "http://lukesmith.xyz/rss.xml" nil 86400)
            ("suckless" "https://suckless.org/atom.xml" nil 86400)
            ("ramsdenj" "https://ramsdenj.com/feed.xml" nil 86400)
            ("jdhao" "https://jdhao.github.io/index.xml" nil 86400)
            ("blog" "http://blog.qutebrowser.org/feeds/all.rss.xml" nil 86400)
            ("edwinwenink" "https://www.edwinwenink.xyz/index.xml" nil 86400)
            ("icyphox" "https://icyphox.sh/blog/feed.xml" nil 86400)
            ("sivers" "https://sivers.org/podcast.rss" nil 86400)
            ("Codemadness" "http://www.codemadness.org/atom.xml" nil 86400)
            ("xkcd" "https://xkcd.com/atom.xml" nil 86400)
            ("planet" "https://planet.emacslife.com/atom.xml" nil 86400)
            ("r/emacs" "https://www.reddit.com/r/emacs/.rss" nil 86400)
            ("andreyor" "https://andreyor.st/feed.xml" nil 86400))))

(use-package eww
  :defer t
  :custom (eww-retrieve-command nil)
  :config
  (defun my/eww-readablity ()
    (interactive)
    (let ((eww-retrieve-command '("rdrview" "-H")))
      (eww-reload nil nil))))
(use-package ntfy :no-require t
  :commands (ntfy-send-notification ntfy-me-at)
  :requires plz
  :config
  (defun ntfy-send-notification (string)
    (interactive (list (read-string "What should we send?:")))
    (plz 'post "https://ntfy.sh/gavin-emacs-notifications"
         :headers '(("Title"  . "Sent From Emacs")
                    ("Tags" . "computer")
                    ("Markdown" . "yes"))
         :body string))
  (defun ntfy-me-at (time string)
    "Send me a notification at this time with this string"
    (interactive (list
                  (read-string "Insert time to remind you" "9:00am")
                  (read-string "What should we send?:")))
    (require 'plz)
    (run-at-time time nil
                 #'plz
                 'post
                 "https://ntfy.sh/gavin-emacs-notifications"
                 :headers '(("Title"  . "Sent From Emacs")
                            ("Tags" . "computer")
                            ("Markdown" . "yes"))
                 :body string)))

(use-package gptel
  :ensure t
  :bind (("C-x <f12>" . gptel-menu)
         ("C-x RET" . gptel-menu)
         ("C-x C-M-i" . gptel-complete))
  :init
  (setq gptel-default-mode #'org-mode)
  (load (locate-user-emacs-file
         "lisp/gptel-complete.el"))
  :config
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*** ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (setq gptel-tools
        (list (gptel-make-tool
               :function (lambda (filepath)
                           (with-temp-buffer
                             (if (string-equal (file-name-extension filepath) "pdf")
                                 (shell-command-to-string (format "pdftotext '%s' -" filepath))
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
                           (let ((full-path (expand-file-name filename path)))
                             (with-temp-buffer
                               (insert content)
                               (write-file full-path))
                             (format "Created file %s in %s" filename path)))
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
               :function #'brave-search-query
               :name "brave_search"
               :description "Perform a web search using the Brave Search API"
               :args (list '(:name "query"
                                   :type string
                                   :description "The search query string"))
               :category "web")
              (gptel-make-tool
               :function (lambda (query)
                           (with-temp-message (concat "Searching Email about " query)
                             (shell-command-to-string
                              (concat "mu find -u -r " query))))
               :name "search_mail"
               :description "Search User Email"
               :args (list
                      '(:name "Email Query"
                              :type string
                              :description "Search term query for email (keep this short)"))
               :category "email")
              (gptel-make-tool
               :function (lambda (query)
                           (with-temp-message (concat "Searching For Documents " query)
                             (shell-command-to-string
                              (concat "recollq -C -n 0-30 " query))))
               :name "search_documents"
               :description "Search for documents and files on this computer"
               :args (list
                      '(:name "Document Query"
                              :type string
                              :description "Search terms query for documents"))
               :category "filesystem")
              (gptel-make-tool
               :function (lambda (regex)
                           (let ((default-directory (project-root (project-current))))
                             (with-temp-message (concat "Searching Codebase "
                                                        default-directory)
                               (shell-command-to-string
                                (concat "rg " regex " " default-directory)))))
               :name "search_codebase"
               :confirm nil
               :description "Search for text in the current programming project"
               :args (list
                      '(:name "Regex"
                              :type string
                              :description "The regex used to search the current codebase"))
               :category "filesystem")))
  (defun brave-search-query (query)
    "Perform a web search using the Brave Search API with the given QUERY."
    (let ((url-request-method "GET")
          (url-request-extra-headers `(("X-Subscription-Token" . ,(password-store--run "show" "brave-api"))))
          (url (format "https://api.search.brave.com/res/v1/web/search?q=%s" (url-encode-url query))))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (when (re-search-forward "^$" nil 'move)
          (let ((json-object-type 'hash-table)) ; Use hash-table for JSON parsing
            (json-parse-string (buffer-substring-no-properties (point) (point-max)))))))))

(use-package pomm
  :defer t
  :custom ((pomm-audio-enabled t))
  :config
  (pomm-mode-line-mode +1))

(use-package typst-ts-mode
  :mode "\\.typ\\'"
  :init
  (unless (package-installed-p 'typst-ts-mode)
    (package-vc-install
     '(typst-ts-mode :url "https://git.sr.ht/~meow_king/typst-ts-mode")))
  :custom
  ;; (optional) If you want to ensure your typst tree sitter grammar version is greater than the minimum requirement
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory)))

(use-package prodigy
  :ensure t
  :bind ("C-x P" . prodigy)
  :config
  (prodigy-define-tag
    :name 'nextjs
    :ready-message " ✓ Ready in [0-9]+ms")
  (prodigy-define-service
    :name "proxima landing_page"
    :command "npm"
    :args '("run" "dev")
    :cwd "/home/gavinok/code/sideprojects/landing_page"
    :port 3000
    :tags '(proxima nextjs)
    :stop-signal 'kill)

  (setq prodigy-vc-authn-project "/home/gavinok/code/work/vc-authn-oidc")
  (prodigy-define-service
    :name "build vc-authn"
    :command (concat prodigy-vc-authn-project
                     "/docker/manage")
    :args '("build")
    :cwd (concat prodigy-vc-authn-project "/docker/")
    :tags '(build work vc-authn))
  (prodigy-define-service
    :name "vc-authn"
    :command (concat prodigy-vc-authn-project
                     "/docker/manage")
    :args '("start")
    :cwd (concat prodigy-vc-authn-project "/docker/")
    :tags '(work vc-authn)
    :ready-message "controller-1     | [-0-9:_TZ\.]+ \\[info     \\] Application startup complete.  \\[uvicorn.error\\]"
    :env `(("DEBUGGER" "false")
           ("ACAPY_ENDPOINT" "http://host.docker.internal:8050")
           ("LOG_WITH_JSON" "FALSE")
           ("NGROK_AUTH" ,(password-store-get "NGROK_AUTH")))
    :stop-signal 'kill)
  (prodigy-define-service
    :name "vc-authn demo"
    :command "docker"
    :args '("compose" "up")
    :cwd (concat prodigy-vc-authn-project "/demo/vue")
    :port 8080
    :tags '(work vc-authn)
    :ready-message "controller-1     | [-0-9:_TZ\.]+ \\[info     \\] Application startup complete.  \\[uvicorn.error\\]"
    :stop-signal 'kill)

  (setq prodigy-endorser-project "/home/gavinok/code/work/aries-endorser-service/")
  (prodigy-define-service
    :name "build endorser"
    :command (concat prodigy-endorser-project
                     "/docker/manage")
    :args '("build")
    :cwd (concat prodigy-endorser-project "/docker/")
    :tags '(build work endorser)
    :env `(("DEBUGGER" "false")
           ("ENDORSER_ENV" "testing")
           ("ACAPY_ENDPOINT" "http://host.docker.internal:8050")
           ("NGROK_AUTH" ,(password-store-get "NGROK_AUTH")))
    :stop-signal 'kill)
  (prodigy-define-service
    :name "endorser"
    :command (concat prodigy-endorser-project
                     "/docker/manage")
    :args '("start-bdd" "--logs")
    :cwd (concat prodigy-endorser-project "/docker/")
    :url "http://localhost:5050/endorser/docs"
    :tags '(work endorser)
    :env `(("DEBUGGER" "false")
           ("ENDORSER_ENV" "testing")
           ("ACAPY_ENDPOINT" "http://host.docker.internal:8050")
           ("NGROK_AUTH" ,(password-store-get "NGROK_AUTH"))
           ("LEDGER_URL" "http://test.bcovrin.vonx.io")
           ("TAILS_SERVER_URL" "https://tails-test.vonx.io"))
    :stop-signal 'kill)
  (prodigy-define-service
    :name "test endorser"
    :command (concat prodigy-endorser-project
                     "/docker/manage")
    :args '("run-bdd-docker")
    :cwd (concat prodigy-endorser-project "/docker/")
    :url "http://localhost:5050/endorser/docs"
    :tags '(test work endorser)
    :env `(("DEBUGGER" "false")
           ("ENDORSER_ENV" "testing")
           ("ACAPY_ENDPOINT" "http://host.docker.internal:8050")
           ("NGROK_AUTH" ,(password-store-get "NGROK_AUTH"))
           ("LEDGER_URL" "http://test.bcovrin.vonx.io")
           ("TAILS_SERVER_URL" "https://tails-test.vonx.io"))
    :stop-signal 'kill)
  (prodigy-define-service
    :name "von"
    :command (concat prodigy-endorser-project
                     "/docker/manage")
    :args '("start")
    :cwd (concat prodigy-endorser-project "/docker/")
    :url "http://localhost:5050/endorser/docs"
    :tags '(work)
    :env `(("DEBUGGER" "false")
           ("ENDORSER_ENV" "testing")
           ("ACAPY_ENDPOINT" "http://host.docker.internal:8050")
           ("NGROK_AUTH" ,(password-store-get "NGROK_AUTH")))
    :stop-signal 'kill))
