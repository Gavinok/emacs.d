;;; Startup
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold to speed up garbage collection
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))
;;; PACKAGE LIST
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; BOOTSTRAP USE-PACKAGE
(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq package-native-compile t)
(setq comp-deferred-compilation t)
;;; ASYNC

;; Emacs look SIGNIFICANTLY less often which is a good thing.
;; asynchronous bytecode compilation and various other actions makes
(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

;;; MY STUFF
(defvar gv/is-termux
      (string-suffix-p
       "Android" (string-trim (shell-command-to-string "uname -a")))
      "Truthy value indicating if emacs is currently running in termux")
(defvar gv/is-terminal
  (not (display-graphic-p))
  "Truthy value indicating if emacs is currently running in a terminal")
(defvar gv/my-system
  (if (string-equal user-login-name "gavinok")
      t
    nil)
  "non-nil value if this is my system")

;; Text to speach script integratoin
(bind-key (kbd "C-x C-M-;")  #'gv/read)
(defun gv/read (&optional ARGS)
  "text to speech"
  (interactive)
  (async-shell-command
   (concat "tts.sh " (shell-quote-argument (x-get-clipboard)))))

(defun gv/scroll-down (arg)
  "Move cursor down half a screen"
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (next-line dist)))

(defun gv/scroll-up (arg)
  "Move cursor down half a screen"
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (previous-line dist)))

(global-set-key (kbd "C-v") #'gv/scroll-down)
(global-set-key (kbd "M-v") #'gv/scroll-up)
;;; General Key Bindings
(use-package crux
  :ensure t
  :bind (;; Remove whitespace when killing at the end of a line
         ([remap kill-line] . crux-kill-and-join-forward)
         ;; Since C-j is so similar
         ("C-x w v" . crux-swap-windows)
         ("C-S-o" . crux-smart-open-line-above)
         ("C-o" . crux-smart-open-line)
         ("M-k" . crux-kill-whole-line)
         :map dired-mode-map
         ("O" . crux-open-with)))

(use-package simple
  :ensure nil
  :bind (("M-SPC" . cycle-spacing)))

;;; UNDO
;; Vim style undo not needed for emacs 28
(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo)
  :defer nil
  :bind (;; I hate it when I accidently use these
         ("C-x u"   . undo-fu-only-undo)
         ("C-/"     . undo-fu-only-undo)
         ("C-x C-u" . undo-fu-only-redo)
         ("C-?"     . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :init (global-undo-fu-session-mode))

;;; TERMINAL SETTINGS
(when gv/is-terminal
  (progn (set-face-background 'default "undefinded")
         (add-to-list 'term-file-aliases
                      '("st-256color" . "xterm-256color"))
         (xterm-mouse-mode t))
  (global-set-key (kbd "<mouse-4>") 'next-line)
  (global-set-key (kbd "<mouse-5>") 'previous-line))

(use-package meow
  :ensure t
  :config
  (defun meow-setup ()
    (setq meow-expand-hint-counts
          '((word . 0) (line . 0) (block . 0) (find . 0) (till . 0)))
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("a" . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     ;; '("a" . meow-append)
     '("b" . meow-back-symbol)
     '("B" . meow-back-word)
     '("c" . meow-change)
     '("d" . meow-kill)
     '("D" . meow-backward-delete)
     '("E" . meow-next-word)
     '("e" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-grab)
     '("G" . meow-cancel-selection)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-append)
     '("j" . meow-next)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("k" . meow-prev)
     '("p" . meow-prev)
     '("P" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("J" . meow-join)
     '("s" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("y" . meow-yank)
     '("q" . meow-quit)
     '("Q" . (lambda () (interactive)
               (delete-overlay mouse-secondary-overlay)))
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("W" . meow-mark-word)
     '("w" . meow-mark-symbol)
     '("V" . meow-line)
     '("x" . meow-line)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)))
  (meow-setup)
  (meow-global-mode))

;;; Aligning Text
(use-package align
  :ensure nil
  :defer t
  :bind ("C-x C-a" . align-regexp)
  :config
  ;; Align using spaces
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))
;;; COMPLETION
(use-package vertico
  :init
;;;; Out Of Order Compleiton
  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless)))
;;;; Extra Completion Functions
  ;; Note that M-sn is used for searghing ang
  (use-package consult
    :defer t
    :bind (("C-c l"       . consult-line)
           ("C-c L"       . consult-line-multi)
           ("C-c i"       . consult-imenu)
           ("C-c o"       . consult-outline)
           ("C-x b"       . consult-buffer)
           ("C-x C-k C-k" . consult-kmacro)
           ;; Used with clipmon makes emacs act as a
           ;; Clipboard Manager
           ("M-y"         . consult-yank-pop)
           ("M-g g"       . consult-goto-line)
           ("M-g M-g"     . consult-goto-line)
           ("C-x C-SPC"   . consult-global-mark)
           ("C-x M-:"     . consult-complex-command)
           ("C-c n"       . consult-org-agenda))
    :custom
    (completion-in-region-function #'consult-completion-in-region)
    :config
    (add-hook 'completion-setup-hook #'hl-line-mode)
    (setq recentf-make-menu-items 150
          recentf-make-saved-items 150)
    (recentf-mode t)
    (use-package marginalia
      :custom
      (marginalia-annotators
       '(marginalia-annotators-heavy marginalia-annotators-light nil))
      :init
      (marginalia-mode)))

;;;; Fuzzy Finding
  (use-package affe
    :bind (("C-c f" . affe-find)
           ("C-c g" . affe-grep)
           ("C-c S-n" . gv/notegrep))
    :commands (affe-grep affe-find)
    :config
    (defun gv/notegrep ()
      "Use interactive grepping to search my notes"
      (interactive)
      (affe-grep org-directory))
    ;; Only exclude git files
    (if (executable-find "fd")
        (setq affe-find-command "fd --hidden")
      ;; else
      (setq affe-find-command
            (concat "find  "
                   "-not -path '*/\\.nnn*' -not -path '*/\\.git*' "
                   "-type f")))

    ;; Manual preview key for `affe-grep'
    (consult-customize affe-grep :preview-key (kbd "M-.")))
  (vertico-mode)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package embark
  :ensure t
  :bind
  (("C-=" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none))))
  )


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;;; THEMEING
(use-package ujelly-theme
  :config
  (load-theme 'ujelly t)
  (global-hl-line-mode t)
  (set-face-background hl-line-face "#111")
  (when gv/my-system
    (set-frame-parameter (selected-frame) 'alpha '(100 100))
    (add-to-list 'default-frame-alist '(alpha 100 100)))
  (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
  (set-face-attribute 'default nil :background "#000" :foreground "#eee")
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 10 :color "#000"))
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 10 :color "#000"))
  (set-face-attribute 'mode-line nil
                      :background  "#0F0F0F")

  ;; vertical padding
  (setq-default header-line-format " "))


;;; WRITING
(use-package writegood-mode
  :hook (flyspell-mode . writegood-mode))

(use-package flyspell-correct
  :bind ("C-c DEL" . flyspell-correct-previous)
  :hook ((org-mode mu4e-compose-mode mail-mode git-commit-mode)
         . turn-on-flyspell))

;;; ORG
(load "~/.emacs.d/lisp/org-config.el")

;;; Git
(use-package magit
  :bind ("C-x g" . magit-status)
  :commands magit)

(use-package diff-hl
  :unless gv/is-termux
  :defer 5
  :init (global-diff-hl-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode))
;;; As You Type Completion
(use-package company
  :defer 3
  :config
  (global-company-mode nil)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)
  ;; tab and go mode
  (company-tng-mode))

;;; VTERM AND ESHELL
(use-package vterm
  :bind ("C-x t" . vterm)
  :commands vterm
  :custom (vterm-max-scrollback 10000)
  :init (when gv/my-system
          (setq term-prompt-regexp ".*ᛋ")))

(use-package esh-autosuggest
  :hook eshell-mode) ;company for eshell

(use-package em-alias
  :ensure nil
  :hook eshell-mode
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "e" "find-file $1")
              (eshell/alias "ee" "find-file-other-window $1"))))

(use-package fish-completion
  :hook eshell-mode
  :when (executable-find "fish")
  :config (global-fish-completion-mode))

;; More accureate color representation than ansi-color.el
(use-package xterm-color
  :ensure t
  :after eshell
  :config
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))

;;; LANGS
(use-package fennel-mode
  :mode "\\.fnl\\'")
(use-package racket-mode
  :mode "\\.rkt\\'")

;; (progn
;;   (use-package flycheck)
;;   (use-package yasnippet :config (yas-global-mode))
;;   (use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
;;     :config (setq lsp-completion-enable-additional-text-edit nil))
;;   (use-package lsp-ui)
;;   ;; Ensure java 11 is installed
;;   (use-package lsp-java
;;     :init
;;     ;; current VSCode defaults
;;     (setq lsp-java-vmargs
;;     '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))
;;     :config (add-hook 'java-mode-hook 'lsp))
;;   (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;;   (use-package dap-java :ensure nil))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(use-package project
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :ensure nil
  :config
  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

    (defun my/project-affe-grep (&optional arg)
      "Use affe-grep instead of emacs's native grep with project.el"
      (interactive)
      (affe-grep (project-root (project-current t)) nil))
  (defun my-git-project-finder (dir)
    "Integrate .git project roots."
    (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
		       (expand-file-name dir))))
      (and dotgit
	   (cons 'transient (file-name-directory dotgit)))))
  (add-hook 'project-find-functions 'my-git-project-finder)
  (setq project-switch-commands
        '((project-find-file "Find file" f)
          (my/project-consult-ripgrep "Ripgrep" g)
          (project-dired "Dired" d)
          (project-vc-dir "VC-Dir" v)
          (project-eshell "Eshell" e)
          (magit-status (project-root (project-current t)) "Magit" m))
        )) ; [built-in] Project Managment

;;; COMPILATION
(use-package compile
  :bind ("C-x M-m" . compile)
  :bind ("C-x C-m" . recompile)
  :config
  (setq compilation-scroll-output t)
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;;; BUFFER MANAGMENT
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . hl-line-mode)
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 40 40 :left :elide)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))
  (setq ibuffer-saved-filter-groups
      '(("home"
         ("Qutebrowser" (name . "qutebrowser"))
         ("emacs-config" (or (filename . ".emacs.d")
                             (filename . "emacs-config")))
         ("Org" (or (mode . org-mode)
                    (filename . "OrgMode")))
         ("Web Dev" (or (mode . html-mode)
                        (mode . css-mode)))
         ("Magit" (name . "\*magit"))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))
         ("Browser" (mode . eaf-mode))
         ("Ement" (name . "\*Ement *")))))
  (add-hook 'ibuffer-mode-hook
          (lambda ()
             (ibuffer-switch-to-saved-filter-groups "home")))) ; [built-in] Powerful interface for managing buffers

;;; ISEARCH
(use-package isearch
  :ensure nil
  :bind (("C-s" . isearch-forward)
         ("C-r" . isearch-backward))
  :config
  (defun isearch-save-and-exit ()
    "Exit search normally. and save the `search-string' on kill-ring."
    (interactive)
    (isearch-done)
    (isearch-clean-overlays)
    (kill-new isearch-string))

  (define-key isearch-mode-map "\M-w" 'isearch-save-and-exit)

  ;; Avoid typing - and _ during searches
  (setq search-whitespace-regexp ".")
  ;; Place cursor at the start of the match similar to vim's t
  ;; C-g will return the cursor to it's orignal position
  (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
  (defun my-goto-match-beginning ()
      (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
        (goto-char isearch-other-end))))

(use-package ffap
  :ensure nil
  :bind ("C-x f" . ffap)
  :init
  (setq find-file-visit-truename t))

;;; Workspace Like Workflow
(use-package perspective
  :ensure t
  :bind (("C-x x s" . persp-switch)
         ("C-x x C-l" . persp-state-load))
  :config
  (persp-mode t))

;;; popup window managment
(use-package popper
  :ensure t ; or :straight t
  :bind (("M-`"   . popper-toggle-latest)
         ("M-~"   . popper-cycle)
         ("C-x M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*xref\\*"
          "\\*Backtrace\\*"
          "^*sly"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  ;; (setq popper-group-function #'popper-group-by-perspective)
  (popper-mode +1))

;;; DEFAULTS
(use-package emacs
  :ensure nil
  :defer nil
  :bind (("C-c w"   . fixup-whitespace)
         ("C-x C-d" . delete-pair)
         ("C-x O"   . other-other-window)
         ("M-c"     . capitalize-dwim)
         ("M-u"     . upcase-dwim)
         ("M-l"     . downcase-dwim)
         ("M-f"     . sim-vi-w))
  :config
  ;; set the title of the frame to the current file - Emacs
  (setq-default frame-title-format '("%b - Emacs"))
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
    (forward-char -1))
  ;; No delay when deleting pairs
  (setq delete-pair-blink-delay 0)

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
  (set-frame-font "Terminus 14" nil t)
;;;; Backups
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        vc-make-backup-files t
        version-control t
        kept-old-versions 0
        kept-new-versions 10
        delete-old-versions t
        backup-by-copying t)
;;;; Defaults
  ;; Cursor Shape
  (setq-default cursor-type 'bar)
  (setq delete-by-moving-to-trash t
        create-lockfiles nil
        auto-save-default nil
        inhibit-startup-screen t
        ring-bell-function 'ignore)
  (setq global-mark-ring-max 100)
;;;; UTF-8
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
;;;; Remove Extra Ui
  (blink-cursor-mode -1)
  (menu-bar-mode -1)               ; To disable the menu bar, place the following line in your .emacs file:
  (unless gv/is-termux
    (scroll-bar-mode -1))          ; To disable the scroll bar, use the following line:
  (tool-bar-mode -1)               ; To disable the toolbar, use the following line:
  (fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"
  (show-paren-mode 1)              ; Highlight parenthesis
  (setq x-select-enable-primary t) ; use primary as clipboard in emacs
  ;; avoid leaving a gap between the frame and the screen
  (setq-default frame-resize-pixelwise t)
;;;; Vim like scrolling
  (setq scroll-step            1
        scroll-conservatively  10000)
  (setq next-screen-context-lines 5)
  ;; move by logical lines rather than visual lines (better for macros)
  (setq line-move-visual nil)
  (fringe-mode))

(use-package autorevert
  :ensure nil
  :defer 1
  :init (global-auto-revert-mode t))

(use-package savehist
  :defer t
  :init (savehist-mode t)) ; Save command history

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)
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
(use-package outline
  :ensure nil
  :defer nil
  :hook (prog-mode . outline-minor-mode)
  :init
  (setq outline-minor-mode-prefix "\C-c"))

;;; enhanced eww
(use-package shrface
  :ensure t
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t)
  (define-key eww-mode-map (kbd "<tab>") 'shrface-outline-cycle)
  (define-key eww-mode-map (kbd "<backtab>") 'shrface-outline-cycle-buffer))

(use-package eww
  :defer t
  :bind ("C-x w w" . eww)
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(use-package nov
  :ensure t
  :defer t
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))


;;;; Setup Folding For Programming
(use-package prog-mode
  :ensure nil
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (setq show-trailing-whitespace t)))
  (add-hook 'prog-mode-hook 'electric-pair-local-mode)
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
;;;;; Smart Indentation
  (defun infer-indentation-style ()
    ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
    ;; neither, we use the current indent-tabs-mode
    (let ((space-count (how-many "^  " (point-min) (point-max)))
          (tab-count (how-many "^\t" (point-min) (point-max))))
      (if (> space-count tab-count) (setq indent-tabs-mode nil))
      (if (> tab-count space-count) (setq indent-tabs-mode t))))
  (add-hook 'prog-mode-hook 'infer-indentation-style))

(use-package clhs
  :bind (:map help-map
              ("C-l" . clhs-doc)))
;;; EXTRA UI
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package pulse
  :unless gv/is-terminal
  :defer t
  :init (defun pulse-line (&rest _)
          (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(;; scroll-up-command
                     ;; scroll-down-command
                     recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))) ; Highlight cursor postion after movement

;;;; Display hex colors in emacs
(use-package rainbow-mode
  :defer t
  :commands (rainbow-mode))

;;; DIRED
(use-package dired
  :ensure nil
  :commands (dired)
  :bind (:map dired-mode-map
              ("-" . dired-up-directory))
  :config
  (setq dired-listing-switches "-aghoA --group-directories-first")
  (add-hook 'dired-mode-hook #'hl-line-mode)
;;;;; Hide . and .. in dired
  (setq dired-omit-files
        (rx (or (seq bol (? ".") "#")
                (seq bol "." eol)
                (seq bol ".." eol))))

  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
;;;;; xdg-open integration
  (require 'dired-x)
  (defun gv/dired-xdg-open ()
    "Open the file at point with xdg-open."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file)
      (message "Opening %s done" file)))
  (define-key dired-mode-map (kbd "O") 'gv/dired-xdg-open)
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

;;; MODELINE
(unless gv/is-termux
  (require 'battery))
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) " "))
            right)))
(unless gv/is-terminal
  (setq-default left-margin-width 2)
  (setq-default right-margin-width 2))
(set-window-buffer nil (current-buffer))
(setq-default mode-line-format
      '((:eval
         (format-mode-line
          (simple-mode-line-render
           ;; Left
           '(" "
             (:eval (propertize
                     (if (and (not buffer-read-only) (buffer-modified-p))
                         "● "
                       "  " ) 'face 'error))
             mode-line-buffer-identification
             ;; value of current line number
             " %l:%c"
             (:eval (propertize
                     (concat " %p%%" " "
                             ;; (if god-local-mode " 😇 " "  ")
                             " ( %m ) ") 'face 'shadow))
             mode-line-misc-info
             )
           ;; Right
           '((:eval (propertize
                     (format-time-string "%a, %b %d %I:%M%p")
                     'face 'font-lock-keyword-face))
             " "
             (:eval (unless gv/is-termux
                      (battery-format
                       "[%p]"
                       (funcall battery-status-function))) )
             "    "))))))

;;; Server Setup
(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;;;; Better PDFs
;https://github.com/politza/pdf-tools
; annotate pdfs with c-c c-a
; hl with c-c c-a h
; for help M-x pdf-tools-help RET
;; (use-package pdf-tools
;;   :mode "\\.pdf\\'"
;;   :config
;;   ;; pdf auto refresh
;;   (add-hook 'doc-view-mode-hook 'auto-revert-mode))

;;;; Torrents
(use-package transmission
  :defer t
  :commands transmission
  :config
  (add-hook 'transmission-mode-hook 'hl-line-mode))

;;;; proced [built-in] htop alternative
(use-package proced
  :ensure nil
  :commands proced
  :config
  (setq proced-auto-update-flag t
        proced-auto-update-interval 2
        proced-decend t))

;;; mu4e
(load "~/.emacs.d/lisp/mu4e-config.el")

;;; EXWM
;; (load "~/.emacs.d/lisp/exwm-config.el")

;; (use-package websocket
;;   :ensure t)
;; (use-package obs-websocket
;;   :ensure nil
;;   :load-path "~/.emacs.d/lisp/obs-websocket.el/obs-websocket.el"
;;   :config
;;   (defun obs-sel-scene (&optional arg)
;;     (interactive)
;;     (unless obs-websocket
;;       (obs-websocket-connect))
;;     (let ((scene (completing-read "Select OBS Command"
;;                                   '("Intermission"
;;                                     "Desktop"))))
;;       (obs-websocket-send "SetCurrentScene" :scene-name scene))))

(use-package keycast
  :ensure t
  :commands (keycast-mode)
  :config
  (set-face-attribute 'keycast-key nil
                      :weight 'normal
                      :box '(:line-width 10 :color "#000")
                      :foreground "000"
                      :background  "#222"))

;; use emacs as a clipboard manager
(use-package clipmon
  :unless (and gv/is-termux (not (executable-find "clipmon")))
  :defer 5
  :config
  (clipmon-mode-start))

(use-package vlf
  :ensure t
  :defer t) ; help emacs handle large files to avoid exwm from locking

;;; Winner Mode
(use-package winner
  :ensure nil
  :bind (("s-/" . winner-undo)
         ("s-?" . winner-redo))
  :config
  :init (winner-mode 1)) ; Window Managment Undo
;;;; Use emacs instead of dmenu
(setenv "LAUNCHER" "emenu -p ")
(setenv "EDITOR" "emacsclient")
;;; Stuff To Ignore
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Install and load `quelpa-use-package'.
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)
;; Install `plz' HTTP library (not on MELPA yet).
(use-package plz
    :quelpa (plz :fetcher github :repo "alphapapa/plz.el"))
;; Install Ement.
(use-package ement
  :commands (ement-connect)
  :quelpa (ement :fetcher github :repo "alphapapa/ement.el")
  :init
  (setq ement-room-sender-headers t
        ement-room-retro-loading t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "3a9f65e0004068ecf4cf31f4e68ba49af56993c20258f3a49e06638c825fbfb6" default))
 '(jumplist-hook-commands
   '(consult-line consult-line-multi consult-grep consult-ripgrep consult-outline affe-find affe-grep dired-jump isearch-forward end-of-buffer beginning-of-buffer find-file))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(stumpwm-mode iceberg-theme csv-mode dogears eglot plain-org-wiki phi-search visual-regexp kakoune ement plz websocket websockets popper yasnippet flycheck evil-terminal-cursor-changer vdiff perspective ob-async kubel groovy-mode consult-lsp lsp-java lsp-ui hydra lsp-mode projectile evil-lion evil-surround evil-commentary evil-collection evil jest-test-mode typescript-mode jest markdown-mode centered-window cheat-sh dot-mode ob-elm ob-hy ob-rust hl-todo rainbow-delimiters modus-operandi-theme all-the-icons-dired highlight-indent-guides typing-game c-c-combo corfu xah-fly-keys academic-phrases selected system-packages goto-chg writegood-mode which-key vterm vlf vimrc-mode vertico undo-fu-session undo-fu ujelly-theme tree-sitter-langs transmission rainbow-mode racket-mode quelpa-use-package pdf-tools pcre2el password-store outline-minor-faces org-superstar org-roam org-plus-contrib org-mime org-download org-alert orderless multiple-cursors modus-themes message-attachment-reminder marginalia magit lua-mode keycast jumplist god-mode flyspell-correct fish-completion fennel-mode expand-region esh-autosuggest epc eaf diff-hl dashboard crux bicycle beacon all-the-icons affe))
 '(persp-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
