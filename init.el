;; -*- lexical-binding: t -*-
;;; Startup
;; Minimize garbage collection during startup

;; useful for quickly debugging emacs
;; (setq debug-on-error t)
(setq running-in-wsl (executable-find "wslpath"))

(when running-in-wsl
  (executable-find "wslpath")

  (setq wls-copy-process nil)

  (defun wls-copy (text)
    (setq wls-copy-process (make-process :name "clip.exe"
					 :buffer nil
					 :command '("clip.exe")
					 :connection-type 'pipe))
    (process-send-string wls-copy-process text)
    (process-send-eof wls-copy-process))

  (setq interprogram-cut-function 'wls-copy))

(setq initial-scratch-message nil)

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)
;; Lower threshold to speed up garbage collection
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old)
             (setq gc-cons-threshold (* 2 1000 1000))
             (setq gc-cons-percentage 0.1))
          t)
;;; Backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
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
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; BOOTSTRAP USE-PACKAGE
(package-initialize)

(eval-when-compile (require 'use-package))
(setq use-package-verbose t
      comp-async-report-warnings-errors nil
      comp-deferred-compilation t)

;; Install and load `quelpa-use-package'.
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;; Keep custom-set-variables and friends out of my init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; ASYNC
;; Emacs look SIGNIFICANTLY less often which is a good thing.
;; asynchronous bytecode compilation and various other actions makes
(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1))

(use-package savehist
  :defer 2
  :init (savehist-mode t))

(use-package repeat
  :defer 10
  :init
  (repeat-mode +1))

;;; MY STUFF
(use-package custom-variables
  :ensure nil :no-require t   :demand t
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
    t
    "Non-nil value if this is my system."))
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
         :map image-mode-map
         ("&"       . my/shell-command-on-file)
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
            (T                              'font-lock-function-name-face))
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

  (defun my/center-pixel-wise (arg)
    (interactive "P")
    (let* ((win-pixel-edges (window-pixel-edges (selected-window)))
           (delta  (- (/ (+ (nth 1 win-pixel-edges) (nth 3 win-pixel-edges)) 2)
	              (cdr (window-absolute-pixel-position (point))))))
      (message "%s %s" delta max-height)
      (pixel-scroll-precision-interpolate delta nil 1)))

  (defun my/scroll-down (arg)
    "Move cursor down half a screen ARG times."
    (interactive "p")
    (let ((dist (/ (window-height) 2)))
      (next-line dist)))

  (defun my/scroll-up (arg)
    "Move cursor up half a screen ARG times."
    (interactive "p")
    (let ((dist (/ (window-height) 2)))
      (previous-line dist)))

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

  (defun echo-eqn-to-tex (eqn-expr &optional arg)
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
    (flet ((file->lines (file-name)
                        (split-string
                         (with-temp-buffer
                           (insert-file-contents-literally file-name)
                           (buffer-substring-no-properties (point-min) (point-max))
                           "\n"))))
      (completing-read
       "All History: "
       (append shell-command-history
               compile-history
               (when (boundp 'eshell-history-file-name)
                 (file->lines eshell-history-file-name))
               (file->lines "~/.bash_history")))))

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
    (let ((proc (make-process :name "Gist Creation"
                              :buffer "*Gist URL*"
                              :command (cl-remove :skip
                                                  (list "gh" "gist" "create"
                                                        "--filename" fname
                                                        "--desc" desc
                                                        (if private
                                                            :skip
                                                          "--public")
                                                        "-"))
                              :sentinel (lambda (process event)
                                          "Listens for process finish and prints the gist's URL"
                                          (when (string-match "finis.*" event)
                                            (message "Gist for file %s created at %s"
                                                     fname
                                                     (with-current-buffer (process-buffer process)
                                                       (goto-char (point-max))
                                                       (thing-at-point 'line))))))))

      (process-send-string proc (buffer-substring BEG END))
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
;; (use-package undo-fu
;;   :ensure t
;;   :bind (("C-x u"   . undo-fu-only-undo)
;;          ("C-/"     . undo-fu-only-undo)
;;          ("C-z"     . undo-fu-only-undo)
;;          ("C-S-z"   . undo-fu-only-redo)
;;          ("C-x C-u" . undo-fu-only-redo)
;;          ("C-?"     . undo-fu-only-redo)))
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
  :defer nil
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
         ("M-1" . delete-other-windows)
         ("M-2" . split-window-below)
         ("M-3" . split-window-right))

  :config
  ;; Set the title of the frame to the current file - Emacs
  (setq-default frame-title-format '("%b - Emacs"))

  ;; How I like my margins
  (unless my/is-terminal
    (setq-default left-margin-width 2)
    (setq-default right-margin-width 2))

  ;; No delay when deleting pairs
  (setq-default delete-pair-blink-delay 0)
  (blink-cursor-mode -1)
  ;; change truncation indicators
  (define-fringe-bitmap 'right-curly-arrow
    [#b10000000
     #b10000000
     #b01000000
     #b01000000
     #b00100000
     #b00100000
     #b00010000
     #b00010000
     #b00001000
     #b00001000
     #b00000100
     #b00000100])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00000100
     #b00000100
     #b00001000
     #b00001000
     #b00010000
     #b00010000
     #b00100000
     #b00100000
     #b01000000
     #b01000000
     #b10000000
     #b10000000])
  (unless my/is-termux
    (fringe-mode))

;;;; Defaults
  ;; Handle long lines
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq-default bidi-inhibit-bpa t)
  (global-so-long-mode 1)

  (setq-default history-length 1000
                use-dialog-box nil
                delete-by-moving-to-trash t
                create-lockfiles nil
                auto-save-default nil
                inhibit-startup-screen t
                ring-bell-function 'ignore)

;;;; UTF-8
  (prefer-coding-system 'utf-8)
;;;; Remove Extra Ui
  (fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"
  (show-paren-mode 1)              ; Highlight parenthesis
  (if running-in-wsl
      (setq-default x-select-enable-primary t) ; use primary as clipboard in emacs
    (setq x-select-enable-primary nil) ; use primary as clipboard in emacs
    )
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

  ;; recentf
  (customize-set-value 'recentf-make-menu-items 150)
  (customize-set-value 'recentf-make-saved-items 150))

(use-package font-setup :ensure nil :no-require t
  :demand t
  :when my/my-system
  :hook ((text-mode . pragmatapro-lig-mode)
         (prog-mode . pragmatapro-lig-mode))
  :init
  ;; Fonts
  ;; The concise one which relies on "implicit fallback values"
  (use-package fontaine
    :unless my/is-terminal
    :config
    (setq fontaine-presets
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
  (load (concat user-emacs-directory
                "lisp/pragmatapro-lig.el"))
  (require 'pragmatapro-lig)
  ;; Enable pragmatapro-lig-mode for specific modes
  (set-fontset-font t 'unicode
                    "PragmataPro Mono:pixelsize=19:antialias=true:autohint=true"
                    nil 'append))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (vue "https://github.com/ikatyang/tree-sitter-vue")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))
(use-package yaml-ts-mode :mode (("\\.yml\\'" . yaml-ts-mode)))
(use-package toml-ts-mode :mode (("\\.toml\\'" . toml-ts-mode)))
(use-package json-ts-mode :mode (("\\.json\\'" . json-ts-mode)))
(use-package sh-script :mode (("\\.sh\\'" . bash-ts-mode)))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
	(bash-mode . bash-ts-mode)
	(js2-mode . js-ts-mode)
	(javascript-mode . js-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(json-mode . json-ts-mode)
	(css-mode . css-ts-mode)
	(python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)))

(use-package unified-marks :ensure nil :no-require t
  :custom
  (global-mark-ring-max 256)
  (set-mark-command-repeat-pop 256)
  :init
  ;; Unify Marks
  (defun my/push-mark-global (&optional location nomsg activate)
    "Always push to the global mark when push-mark is called"
    (let ((old (nth global-mark-ring-max global-mark-ring))
          (history-delete-duplicates nil))
      (add-to-history
       'global-mark-ring (copy-marker (mark-marker))
       global-mark-ring-max t)
      (when old
        (set-marker old nil))))
  (advice-add 'push-mark :after #'my/push-mark-global))
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
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))

;;; COMPLETION
(use-package vertico
  :init
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless flex)))
  (load (concat user-emacs-directory
                "lisp/affe-config.el"))
  (use-package marginalia
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
  (setq enable-recursive-minibuffers t))
;;;; Extra Completion Functions
(use-package consult
  :after vertico
  :bind (("C-x b"       . consult-buffer)
         ("C-x C-k C-k" . consult-kmacro)
         ("M-y"         . consult-yank-pop)
         ("M-g g"       . consult-goto-line)
         ("M-g M-g"     . consult-goto-line)
         ("M-g f"       . consult-flymake)
         ("M-g i"       . consult-imenu)
         ("M-s l"       . consult-line)
         ("M-s L"       . consult-line-multi)
         ("M-s u"       . consult-focus-lines)
         ("M-s g"       . consult-ripgrep)
         ("M-s M-g"     . consult-ripgrep)
         ("C-x C-SPC"   . consult-global-mark)
         ("C-x M-:"     . consult-complex-command)
         ("C-c n"       . consult-org-agenda)
         ("C-c m"       . my/notegrep)
         :map minibuffer-local-map
         ("M-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (defun my/notegrep ()
    "Use interactive grepping to search my notes"
    (interactive)
    (consult-ripgrep org-directory))
  (recentf-mode t))
(use-package consult-dir
  :ensure t
  :bind (("C-x C-j" . consult-dir)
         ;; :map minibuffer-local-completion-map
         :map vertico-map
         ("C-x C-j" . consult-dir)))
(use-package consult-recoll
  :bind (("M-s r" . consult-recoll)
         ("C-c I" . recoll-index))
  :init
  (setq consult-recoll-inline-snippets t)
  :config
  (defun recoll-index (&optional arg) (interactive)
    (start-process-shell-command "recollindex"
                                 "*recoll-index-process*"
                                 "recollindex")))

(use-package embark
  :ensure t
  :bind
  ;; pick some comfortable binding
  (("C-="                     . embark-act)
   ("C-<escape>"              . embark-act)
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-d"                     . dragon-drop)
   :map embark-defun-map
   ("M-t" . chatgpt-gen-tests-for-region)
   :map embark-general-map
   ("M-c" . chatgpt-prompt)
   :map embark-region-map
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
  (defun search-in-source-graph (text))
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
                                 (concat "dragon-drop " file))))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (:all embark consult)
  :demand t
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
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

;;;; Code Completion
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.3)
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
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))
(use-package cape
  :defer 10
  :bind ("C-c f" . cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
  (cl-pushnew #'cape-file completion-at-point-functions)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package yasnippet
  :ensure t
  :init
  (setq yas-nippet-dir "~/.emacs.d/snippets")
  (yas-global-mode))
(use-package yasnippet-snippets
  :ensure t :after yasnippet)
(use-package yasnippet-capf
  :ensure nil
  :quelpa (yasnippet-capf :fetcher github :repo "elken/yasnippet-capf")
  :after yasnippet
  :hook ((prog-mode . yas-setup-capf)
         (text-mode . yas-setup-capf)
         (lsp-mode  . yas-setup-capf)
         (sly-mode  . yas-setup-capf))
  :bind (("C-c y" . yasnippet-capf)
         ("M-+"   . yas-insert-snippet))
  :config
  (defun yas-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'yasnippet-capf
                      completion-at-point-functions)))
  (push 'yasnippet-capf completion-at-point-functions))

;;; THEMEING
(use-package spaceway-theme
  :ensure nil
  :load-path "lisp/spaceway/"
  :config
  (global-hl-line-mode t)
  (set-frame-parameter nil 'cursor-color "#dc322f")
  (add-to-list 'default-frame-alist '(cursor-color . "#dc322f"))

  (when my/my-system
    (set-frame-parameter nil 'alpha-background 80)
    (add-to-list 'default-frame-alist '(alpha-background . 80)))

  (load-theme 'spaceway t)
  (setenv "SCHEME" "dark")
  )

(use-package doom-themes
  :disabled t
  :demand t
  :ensure t
  :config
  (load-theme 'doom-earl-grey t)
  (set-face-attribute 'default nil :foreground "#333")
  (global-hl-line-mode t)
  (setenv "SCHEME" "light"))
;;; WRITING
(use-package writegood-mode
  :hook ((markdown-mode nroff-mode org-mode
                        mail-mode
                        git-commit-mode)
         . writegood-mode))

;; (use-package lsp-grammarly
;;   :ensure t
;;   :hook (writegood-mode . (lambda ()
;;                        (require 'lsp-grammarly)
;;                        (lsp-deferred))))
                                        ; or lsp-deferred

(use-package writeroom-mode
  :commands (writeroom-mode global-writeroom-mode)
  :init
  (setq writeroom-width 90))

(use-package jinx
  :demand t
  :ensure t
  :bind ("C-c DEL" . jinx-correct)
  :init
  (global-jinx-mode)
  (add-to-list 'ispell-skip-region-alist '("+begin_src" . "+end_src"))
  (setopt flyspell-use-meta-tab nil))

;;; ORG
(load (concat user-emacs-directory
              "lisp/org-config.el"))

;;; Email
(load (concat user-emacs-directory
              "lisp/mu4e-config.el"))

;;; ChatGPT
;; (use-package chatgpt
;;   :load-path "lisp/"
;;   :ensure nil
;;   :bind (
;; ))

;;; Git
(use-package magit
  :bind (("C-x v SPC" . magit-status)
         :map project-prefix-map
         ("m" . project-magit))
  :commands (magit project-magit)
  :config
  (add-to-list 'project-switch-commands
               '(project-magit "Magit" m))
  (defun project-magit  ()
    (interactive)
    (let ((dir (project-root (project-current t))))
      (magit-status dir))))

(use-package forge :ensure t :after magit)
(use-package ediff
  :after (magit vc)
  :commands (ediff)
  :init
  ;; multiframe just doesn't make sense to me
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook 'winner-undo))
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl
  :unless my/is-termux
  :defer 5
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-pre-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode)
  :config (diff-hl-flydiff-mode))

;;; VTERM AND ESHELL
(use-package vterm
  :bind (("C-x t" . vterm)
         :map vterm-mode-map
         ("M-p" . vterm-send-up)
         ("M-n" . vterm-send-down))

  :commands vterm
  :custom (vterm-max-scrollback 10000)
  :init (when my/my-system
          (setopt term-prompt-regexp ".*ᛋ")))

;; (use-package with-editor
;;   :hook ((shell-mode-hook eshell-mode-hook term-exec-hook vterm-exec-hook)
;;          . with-editor-export-editor)
;;   :bind (([remap async-shell-command] . with-editor-async-shell-command)
;;          ([remap shell-command] . with-editor-shell-command)))

(use-package eshell
  :bind ("C-x E" . eshell))

(use-package em-alias
  :ensure nil
  :after eshell
  :config
  
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "e" "find-file $1")
              (eshell/alias "ee" "find-file-other-window $1")
              (eshell/alias "v" "view-file $1")
              (eshell/alias "o" "crux-open-with $1")))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "e" "find-file $1")
              (eshell/alias "ee" "find-file-other-window $1")
              (eshell/alias "v" "view-file $1")
              (eshell/alias "o" "crux-open-with $1"))))

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

;; (use-package fish-completion
;;   :demand t
;;   :config
;;   (global-fish-completion-mode))

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
         ("d" . project-dired))
  :init
  (setopt project-switch-commands
          '((project-find-file "Find file" ?f)
            (project-dired "Dired" ?d)
            (project-vc-dir "VC-Dir" ?v)
            (project-eshell "Eshell" ?e)
            (project-shell "Shell" ?s)))
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

  (defvar project-root-markers
    '(".git" "spago.dhall" "CMakeList.txt" "package.clj"
      "package.json" "mix.exs" "Project.toml" ".project" "Cargo.toml"
      "qlfile"))

  (defun my/project-find-root (path)
    (let* ((this-dir (file-name-as-directory (file-truename path)))
           (parent-dir (expand-file-name (concat this-dir "../")))
           (system-root-dir (expand-file-name "/")))
      (cond
       ((my/project-root-p this-dir) (cons 'transient this-dir))
       ((equal system-root-dir this-dir) nil)
       (t (my/project-find-root parent-dir)))))

  (defun my/project-root-p (path)
    (let ((results (mapcar (lambda (marker)
                             (file-exists-p (concat path marker)))
                           project-root-markers)))
      (eval `(or ,@ results))))

  (add-to-list 'project-find-functions #'my/project-find-root))

;;; COMPILATION
(use-package compile
  :defer t
  :hook (((c++-mode c-mode java-mode javascript-mode go-mode nroff-mode) . generic-compiler)
         (purescript-mode . spago-compiler))
  :bind (("C-x M-m" . compile)
         ("C-x C-m" . recompile))
  :init
  (defun has-makefile-p ()
    (or (file-exists-p "makefile")
	(file-exists-p "Makefile")))
  (defun spago-compiler ()
    (unless (has-makefile-p)
      (setq-local compile-command "spago run")))
  (defun generic-compiler ()
    (unless (has-makefile-p)
      (setq-local compile-command
		  (concat "compiler "
			  (when buffer-file-name
			    (shell-quote-argument buffer-file-name))))))
  :config
  (setq compilation-scroll-output t)
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (defun generic-compiler ()
    (unless (or (file-exists-p "makefile")
		(file-exists-p "Makefile"))
      (setq-local compile-command
		  (concat "compiler "
			  (if buffer-file-name
			      (shell-quote-argument
			       (file-name-sans-extension buffer-file-name)))))))
  (add-hook 'c++-mode-hook #'generic-compiler)

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
           ("Magit" (name . "\*magit"))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")))
           ("Browser" (mode . eaf-mode))
           ("Ement" (name . "\*Ement *"))
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
         (:map isearch-mode-map
               ("M-w" . isearch-save-and-exit)
               ("M-R" . isearch-query-replace)
               ("M-/" . isearch-complete))
         ;; (:repeat-map isearch-repeat-map
         ;; ("s" . isearch-repeat-forward))
         )
  :custom ((isearch-lazy-count t)
           (lazy-count-prefix-format nil)
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

;;; Workspace Like Workflow
(use-package perspective
  :ensure t
  :bind-keymap
  ("C-c p" . perspective-map)
  :bind (:map perspective-map
         ("s" . persp-switch)
         ("C-l" . persp-state-load)
         ("B" . persp-switch-to-scratch-buffer))
  :config
  (persp-mode t))

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
          "\\*compilation\\*"
          "\\*rustic-"
          "^*tex"
          "\\*Ement Notifications\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Dtache Shell Command\\*"
          "\\*mu4e-update\\*"
          "\\*GDB.*out\\*"
          help-mode
          compilation-mode))
  (setopt popper-display-control 'user)
  (popper-mode +1))

(use-package multiple-cursors
  :bind (("C-M-'" . mc/edit-lines)
         ("C-M-|" . mc/mark-all-in-region-regexp)
         ;; Call with a 0 arg to skip one
         ("C-M-." . mc/mark-next-like-this)
         ("C-M-," . mc/mark-previous-like-this))
  :config
  ;; Use phi-search to replace isearch when using multiple cursors
  (use-package phi-search
    :bind (:map mc/keymap
                ("C-s" . phi-search)
                ("C-r" . phi-search-backward)
                ("C-w" . kill-region)
                ("C-w" . kill-region)))
  (defun toggle-corfu-auto-for-mc (&optional arg)
    (if multiple-cursors-mode
        (corfu-mode -1)
      (corfu-mode 1)))
  (cl-pushnew 'toggle-corfu-auto-for-mc multiple-cursors-mode-hook))

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
        ("<mouse-7>"     . forward-char)
        ;; :map key-translation-map
        ;; ("<mouse-4>"     . "<wheel-up>")
        ;; ("<mouse-5>"     . "<wheel-down>")
        ;; ("<mouse-6>"     . "<wheel-left>")
        ;; ("<mouse-7>"     . "<wheel-right>")
        )
  :init
  (context-menu-mode 1))

(use-package autorevert
  :ensure nil
  :defer 1
  :init (global-auto-revert-mode t))

(use-package savehist
  :defer 2
  :init (savehist-mode t)) ; Save command history

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line)))

;;; FOLDING
(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-<tab>"   . hs-cycle)
              ("<backtab>" . hs-global-cycle))
  :init
  (define-advice hs-toggle-hiding (:before (&rest _) move-point-to-mouse)
    "Move point to the location of the mouse pointer."
    (mouse-set-point last-input-event))
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;; TODO: Fix this case. `hs-show-block' needs to be
             ;; called twice to open all folds of the parent
             ;; block.
             (save-excursion (hs-show-block))
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle (save-excursion (hs-show-all))
                        (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'font-lock-comment-face)
                         (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▾")))))

(use-package outline
  :hook ((prog-mode) . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              ("C-c u" . outline-up-heading)
              ("C-c j" . outline-forward-same-level)
              ("C-c k" . outline-backward-same-level)
              :repeat-map outline-navigation-repeat-map
              ("TAB" . outline-cycle)
              ([tab] . outline-cycle)
              ("S-TAB" . outline-cycle-buffer)
              ([backtab] . outline-cycle-buffer))
  :custom (outline-minor-mode-use-buttons 'in-margins))

;; Automatic code formatting
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  ;; Setup auto formatting for purescript
  (push '(purs-tidy "purs-tidy" "format") apheleia-formatters)
  (setf (alist-get 'purescript-mode apheleia-mode-alist) '(purs-tidy))
  ;; Setup auto formatting for haskell
  (push '(fourmolu "fourmolu") apheleia-formatters)
  (setf (alist-get 'haskell-mode apheleia-mode-alist) '(fourmolu)))

;;; LSP
;; Should boost performance with lsp
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(use-package lsp-mode
  :defer t
  :bind (("C-h ," . help-at-pt-buffer)
         (:map lsp-mode-map
               ("M-<return>" . lsp-execute-code-action))
         (:map c++-mode-map
               ("C-c x" . lsp-clangd-find-other-file))
         (:map c-mode-map
               ("C-c x" . lsp-clangd-find-other-file)))
  :commands (lsp lsp-deferred)
  :init
  ;; (setenv "LSP_USE_PLISTS" "1")
  ;; Increase the amount of data emacs reads from processes
  (setq read-process-output-max (* 3 1024 1024))
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0"
                                  "--clang-tidy"
                                  "--enable-config"))
  ;; General lsp-mode settings
  (setq lsp-completion-provider :none
        lsp-enable-snippet t
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        lsp-diagnostics-provider :flymake
        lsp-keymap-prefix "C-x L"
        lsp-eldoc-render-all t)
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'lsp-capf completion-category-defaults)
                    '((styles . (orderless-flex))))))
  :config
  (defun help-at-pt-buffer ()
    (interactive)
    (let ((help (help-at-pt-kbd-string))
          (h-at-p-buf "*Help At Point*"))
      (if help
          (progn (with-current-buffer (get-buffer-create h-at-p-buf)
                   (view-mode -1)
                   (erase-buffer) (insert (format "%s" (substitute-command-keys help)))
                   (view-mode +1))
                 (switch-to-buffer-other-window h-at-p-buf))
        (if (not arg) (message "No local help at point")))))
  (use-package lsp-ui
    :ensure t
    :after lsp
    :init
    (setq lsp-ui-sideline-show-code-actions t)
    (setq lsp-ui-sideline-show-diagnostics t)))

(use-package lsp-languages
  :no-require t :ensure nil
  :hook ((c-mode          . lsp-deferred)
         (c++-mode        . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (purescript-mode . lsp-deferred)
         (python-mode     . lsp-deferred)
         (python-ts-mode     . lsp-deferred)
         (js-mode         . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :init
  (use-package lsp-javascript :ensure nil :no-require t
    :hook (javascript-mode . lsp-deferred)
    :config
    (setq lsp-javascript-display-inlay-hints t
          lsp-javascript-display-return-type-hints t
          lsp-javascript-display-variable-type-hints t
          lsp-javascript-display-parameter-type-hints t)
    (with-eval-after-load 'js
      (keymap-set js-mode-map "M-." 'xref-find-definitions)))

  (use-package lsp-rust :ensure nil :no-require t
    :when (executable-find "rust-analyzer")
    :hook (rust-mode       . lsp-deferred)
    :config
    (setq lsp-rust-analyzer-inlay-hints-mode 1))

  (use-package lsp-haskell :ensure t
    :hook (haskell-mode    . lsp-deferred))

  ;; (use-package lsp-java :ensure t
  ;;   :hook (java-mode       . lsp-deferred)
  ;;   :init
  ;;   (require 'lsp-java-boot)
  ;;   (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode))

  (use-package lsp-pyright :ensure t
    :hook ((python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp-deferred)))
           (python-ts-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp-deferred))))
    :init
    (setq python-shell-enable-font-lock nil)))

;;; Debugging
(use-package dap-mode
  :ensure t
  :defer t
  :bind (:map dap-mode-map
              ("C-x D D" . dap-debug)
              ("C-x D d" . dap-debug-last))
  :init
  ;; (defun my/dap-cpp-setup ()
  ;;   (require 'dap-gdb-lldb)
  ;;   (dap-gdb-lldb-setup))
  :config
  ;; (my/dap-cpp-setup)
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

;;; Languages
(use-package nvm
  :ensure nil
  :commands (my/nvm-use)
  :quelpa (nvm :fetcher github :repo "rejeep/nvm.el")
  :commands (nvm-use nvm-use-for-buffer)
  :config
  (setq nvm-dir "/home/gavinok/.config/nvm")
  (defun my/nvm-use () (interactive)
    (nvm-use
     (completing-read "Enter Node Version" '("18.16.0" "16.17.1")))))

(use-package extra-languages
  :ensure nil :no-require t
  :init
;;;; Haskell
  (use-package haskell-mode :ensure t :mode "\\.hs\\'"
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
      (setq outline-regexp "\\`\\|\\s-+\\S-")))
;;;; PureScript
  (use-package purescript-mode :ensure t :mode "\\.purs\\'"
    :hook ((purescript-mode . purescript-indent-mode)
           (purescript-mode . turn-on-purescript-unicode-input-method))
    :config
    (use-package psci
      :ensure t
      :hook (purescript-mode . inferior-psci-mode)))

;;;; WEB
  (use-package lsp-tailwindcss
    :init
    (setq lsp-tailwindcss-add-on-mode t))
  (use-package web-mode
    :mode (;; ("\\.ts\\'"  . web-mode)
           ;; ("\\.tsx\\'"  . web-mode)
           ("\\.html\\'" . web-mode)
           ("\\.vue\\'"  . web-mode))
    :hook ((web-mode            . lsp-deferred))
    :bind (
           :map web-mode-map
           ("C-c C-M-f". sgml-skip-tag-forward)
           ("C-c C-M-b". sgml-skip-tag-backward)
           ("C-c C-f". sgml-skip-tag-forward)
           ("C-c C-b". sgml-skip-tag-backward)
           ("C-M-i" . completion-at-point)
           ("C-M-u" . web-mode-element-parent)
           ("C-M-d" . web-mode-element-child))
    :custom
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
    (define-derived-mode typescript-mode web-mode "TypeScript")
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-auto-close-style 1
          web-mode-enable-auto-indentation nil
          web-mode-enable-auto-quoting nil
          web-mode-enable-auto-pairing nil
          web-mode-enable-auto-opening nil))
;;; Rust
  (use-package rust-mode    :ensure t :mode "\\.rs\\'"
    :init
    ;; scratchpad for rust
    (setq lsp-rust-clippy-preference "on")
    (use-package rust-playground
      :commands (rust-playground)
      :ensure t))
;;;; Racket
  (use-package racket-mode  :ensure t :mode "\\.rkt\\'"
    :config
    (require 'racket-xp)
    (add-hook 'racket-mode-hook #'racket-xp-mode)
    (add-hook 'racket-mode-hook #'prettify-symbols-mode)
    (defun setup-racket-eldoc ()
      (eldoc-mode +1)
      (setq eldoc-documentation-function #'racket-xp-eldoc-function))
    (add-hook 'racket-mode-hook #'setup-racket-eldoc))
;;;; Clojure
  (use-package clojure-mode :ensure t :mode "\\.clj\\'")
  )

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode       . infer-indentation-style)
         (prog-mode       . (lambda () (setq-local show-trailing-whitespace t)))
         (emacs-lisp-mode . (lambda () (add-hook 'local-write-file-hooks 'check-parens)))
         (lisp-mode       . (lambda () (setq indent-tabs-mode nil)))
         ;; Make all scripts executable. Ya this might be sketch but I don't
         (after-save      . executable-make-buffer-file-executable-if-script-p))
  :bind (:map emacs-lisp-mode-map
              ("C-c RET" . emacs-lisp-macroexpand)
              ("C-c C-k" . eval-buffer))
  :init
  ;; Don't prompt for a reference
  (setq xref-prompt-for-identifier nil)
  (global-prettify-symbols-mode)

  ;; Smart Indentation
  (defun infer-indentation-style ()
    ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
    ;; neither, we use the current indent-tabs-mode
    (let ((space-count (how-many "^  " (point-min) (point-max)))
          (tab-count (how-many "^\t" (point-min) (point-max))))
      (if (> space-count tab-count) (setq indent-tabs-mode nil))
      (if (> tab-count space-count) (setq indent-tabs-mode t)))))

(use-package emmet-mode
  :hook ((js-jsx-mode typescript-mode) emmet-jsx-major-modes)
  :bind
  ("C-j" . emmet-expand-line)
  (:map emmet-mode-keymap
        ("M-}" . emmet-next-edit-point)
        ("M-{" . emmet-prev-edit-point))
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

;;; Lisp
(use-package sly
  :commands (sly sly-connect)
  :init
  (setq sly-symbol-completion-mode nil
        sly-default-lisp 'roswell
        ros-config (concat user-emacs-directory
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
  :hook (((calc-mode term-mode vterm-mode) . puni-disable-puni-mode)
         (puni-mode  . electric-pair-local-mode))
  :bind (("C-c s" . puni-mode)
         :map puni-mode-map
         ("C-c DEL" . jinx-correct)
         ;; ("C-c DEL" . flyspell-correct-previous)
         ("M-e"   . puni-end-of-sexp)
         ("M-a"   . puni-beginning-of-sexp)
         ("C-M-f" . puni-forward-sexp-or-up-list)
         ("C-M-b" . puni-backward-sexp-or-up-list)
         ("C-)"   . puni-slurp-forward)
         ("C-0"   . puni-slurp-forward)
         ("C-}"   . puni-barf-forward)
         ("C-{"   . puni-barf-backward)
         ("C-("   . puni-slurp-backward)
         ("C-9"   . puni-slurp-backward)
         ("M-("   . puni-wrap-round)
         ("M-R" .   puni-raise)
         ;; ("C-M-j" . sp-join-sexp)
         ("C-M-t" . puni-transpose)
         ;; ("C-M-k" . puni-kill-thing-at-point)
         ("C-M-?" . puni-convolute)
         ("C-k"   . puni-kill-line)
         ("M-k"   . kill-sexp)
         ("M-C"   . puni-clone-thing-at-point)
         ("C-M-z" . puni-squeeze)
         ("C-M-z" . puni-squeeze)
         ("M-<backspace>" . backward-kill-word)
         ("C-w" . kill-region))
  :chords ("fj" . mark-sexp)

  :init
  (puni-global-mode t)
  :config
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
    (default-indent-new-line))

  ;;;; Better Killing And Yanking
  (setq rectangle-mark-mode nil)
  (setq *last-kill-was-rectangle* rectangle-mark-mode)

  (defun remember-last-kill-type (&rest d)
    (setq *last-kill-was-rectangle* rectangle-mark-mode))

  ;; (advice-add 'kill-region :before #'remember-last-kill-type)
  ;; (advice-add 'kill-ring-save :before #'remember-last-kill-type)
  ;; (advice-add 'kill-rectangle :before #'remember-last-kill-type)

  (defun my/kill-region (BEG END &optional REGION)
    (interactive (list (mark) (point) 'region))
    (cond
     (rectangle-mark-mode (kill-rectangle
                           (region-beginning) (region-end)))
     (mark-active (kill-region
                   (region-beginning) (region-end)))
     (t (backward-kill-sexp 1))))

  (defun my/yank (&optional arg) (interactive)
    (if *last-kill-was-rectangle*
        (yank-rectangle)
      (yank arg)))

  ;; Avoid terminal binding conflict
  (unless my/is-termux
    (bind-key (kbd "M-[") #'puni-splice 'puni-mode-map)
    (bind-key (kbd "M-]") #'puni-split 'puni-mode-map)))

(use-package flymake
  :defer 10
  :bind (("M-g d"   . flymake-show-buffer-diagnostics)
         ("M-g M-d" . flymake-show-project-diagnostics)
         ("M-g M-n" . flymake-goto-next-error)
         ("M-g M-p" . flymake-goto-prev-error)
         :repeat-map flymake-repeatmap
         ("p" . flymake-goto-prev-error)
         ("n" . flymake-goto-next-error)
         :map flymake-diagnostics-buffer-mode-map
         ("?" . flymake-show-diagnostic-here)
         :map flymake-project-diagnostics-mode-map
         ("?" . flymake-show-diagnostic-here))
  :hook (prog-mode . (lambda () (flymake-mode t)))
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

(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items nil))

(use-package eldoc
  :defer 10
  :init
  (setq eldoc-echo-area-display-truncation-message t)
  (setq eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode t))

(use-package paren-face
  :ensure t
  :hook ((prog-mode cider-repl-mode eshell-mode
                    fennel-repl-mode geiser-repl-mode inferior-emacs-lisp-mode
                    inferior-lisp-mode inferior-scheme-mode lisp-interaction-mode
                    racket-repl-mode scheme-interaction-mode sly-mrepl-mode)
         . paren-face-mode))

;;; EXTRA UI
(use-package hl-todo :ensure t :hook prog-mode)

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
;;;;; Hide . and .. in dired
  (setq dired-omit-files
        (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))
;;;;; xdg-open integration
  (require 'dired-x)
  ;; prevent opening extra dired buffers
  ;; emacs 28
  (setq dired-kill-when-opening-new-dired-buffer t))

;;; PASS
(use-package password-store
  :commands (password-store-copy
             password-store-insert
             password-store-generate))

;; Authenticte with auth-source-pass
(use-package auth-source-pass
  :after password-store
  :config
  (auth-source-pass-enable))

(use-package tab-bar
  :config
  (defun tab-bar-tab-name-format-comfortable (tab i)
    "Add spacing to tab bar mode"
    (propertize (concat " " (tab-bar-tab-name-format-default tab i) " ")
                'face (funcall tab-bar-tab-face-function tab)))
  (setq tab-bar-tab-name-format-function #'tab-bar-tab-name-format-comfortable)

  (add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)
  ;; TODO Determin a better way to add some of my modeline to the tab bar
  ;; (customize-set-variable 'tab-bar-format (cons #'tab-bar-format-global tab-bar-format))
)

;;; MODELINE
(load (concat user-emacs-directory
              "lisp/modeline.el"))

;;; Server Setup
(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;;;; Better PDFs
;;https://github.com/politza/pdf-tools
;; annotate pdfs with c-c c-a
;; hl with c-c c-a h
;; for help M-x pdf-tools-help RET
;; (load (concat user-emacs-directory
;;               "lisp/exwm-config.el"))
(use-package pdf-tools
  :ensure t :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelabels)
  (setq-default pdf-view-display-size 'fit-page)
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdftools-open link)))))

;;; mu4e
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/")
;; (load "~/.emacs.d/mu4e-config.el")

(use-package keycast
  :ensure t
  :commands (keycast-mode))

;; use emacs as a clipboard manager
(use-package clipmon
  :ensure t :defer 5
  :unless (or my/is-termux (not (executable-find "clipmon")))
  :config
  (clipmon-mode-start))

;;; Winner Mode
(use-package winner
  :ensure nil
  :bind (("s-/" . winner-undo)
         ("s-?" . winner-redo))
  :config
  :init (winner-mode 1)) ; Window Managment Undo

;; Install `plz' HTTP library (not on MELPA yet).
(use-package plz
  :ensure nil
  :when my/my-system
  :quelpa (plz :fetcher github :repo "alphapapa/plz.el")
  :after ement)

;; Install Ement.
(use-package ement
  :ensure t
  :when my/my-system
  :commands (my/ement-connect)
  :init
  (customize-set-variable 'ement-room-message-format-spec "%B%r%R%t")
  (defun my/ement-connect ()
    (interactive)
    (ement-connect :user-id "@gavinok:matrix.org"
                   :password (password-store-get "riot.im/gavinok")))
  )

(use-package hammy
  :ensure nil
  :quelpa (hammy :fetcher github :repo "alphapapa/hammy.el")
  :commands (happy-start hammy-start-org-clock-in)
  :config
  (hammy-mode t)
  (hammy-define (propertize "🍅" 'face '(:foreground "tomato"))
                :documentation "The classic pomodoro timer."
                :intervals
                (list
                 (interval :name "Work"
                           :duration "25 minutes"
                           :before (do (announce "Starting work time.")
                                       (notify "Starting work time."))
                           :advance (do (announce "Break time!")
                                        (notify "Break time!")))
                 (interval :name "Break"
                           :duration (do (if (and (not (zerop cycles))
                                                  (zerop (mod cycles 3)))
                                             ;; If a multiple of three cycles have
                                             ;; elapsed, the fourth work period was
                                             ;; just completed, so take a longer break.
                                             "30 minutes"
                                           "5 minutes"))
                           :before (do (announce "Starting break time.")
                                       (notify "Starting break time."))
                           :advance (do (announce "Break time is over!")
                                        (notify "Break time is over!")))))
  )

(setq pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode t)
(keymap-global-set "C-v" #'my/scroll-down)
(keymap-global-set "M-v" #'my/scroll-up)

;; (use-package direnv
;;   :ensure t
;;   :config
;;   (envrc-global-mode))

(use-package comby
  :when (executable-find "comby"))

(use-package strokes-mode
  :ensure nil
  :bind ("S-<down-mouse-2>" . strokes-do-stroke)
  :commands (strokes-do-stroke strokes-global-set-stroke)
  :init
  (require 'vertico)
  (vertico-mouse-mode t)
  (strokes-mode t))

(use-package docker
  :bind (("C-x d" . docker)))

;; (use-package carp
;;   :load-path "~/.emacs.d/lisp/carp.el"
;;   :init t
;;   :hook (carp-mode ))

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")
(setenv "READER" "emacsclient")
;;; Stuff To Ignore
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(let ((f "lisp/termux.el"))
  (when (file-exists-p f)
    (load (concat user-emacs-directory f))))
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setenv "PATH" (concat (getenv "PATH") ":/home/gavinok/.cargo/bin"))
