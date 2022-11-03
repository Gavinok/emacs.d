;;; Startup
;; Minimize garbage collection during startup
;; (setq gc-cons-threshold most-positive-fixnum)
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)
;; Lower threshold to speed up garbage collection
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old)
             (setq gc-cons-threshold (* 2 1000 1000)))
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
(setq comp-async-report-warnings-errors nil)
(setq comp-deferred-compilation t)

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
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

;;; MY STUFF
(defvar my/is-termux
  (string-suffix-p
   "Android" (string-trim (shell-command-to-string "uname -a")))
  "Truthy value indicating if Emacs is currently running in termux.")
(defvar my/is-terminal
  (not window-system)
  "Truthy value indicating if Emacs is currently running in a terminal.")
(defvar my/my-system
  (if (string-equal user-login-name "gavinok")
      t
    nil)
  "Non-nil value if this is my system.")

;;;###autoload
(defun my/scroll-down (arg)
  "Move cursor down half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (next-line dist)))

;;;###autoload
(defun my/scroll-up (arg)
  "Move cursor up half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (previous-line dist)))

(global-set-key [remap scroll-up-command] #'my/scroll-down)
(global-set-key [remap scroll-down-command] #'my/scroll-up)

;;;###autoload
(defun my/shell-command-on-file (command)
  "Execute COMMAND asynchronously on the current file."
  (interactive (list (read-shell-command
                      (concat "Async shell command on " (buffer-name) ": "))))
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (async-shell-command (concat command " " filename))))
(bind-key (kbd "C-M-&") #'my/shell-command-on-file)
(with-eval-after-load 'image-mode
  (bind-key (kbd "&") #'my/shell-command-on-file 'image-mode-map))

;;;###autoload
(defun insert-iota (&optional arg)
  (interactive)
  (let ((limit (cl-parse-integer (completing-read "Insert Up To: " nil))))
    (mapcar (lambda (x) (insert (format " %d" x))) (-iota limit))))
;;; Defaults
(use-package emacs
  :ensure nil
  :defer nil
  :bind (("C-x u"   . undo-only)
         ("C-/"     . undo-only)
         ("C-z"     . undo-only)
         ("C-S-z"   . undo-redo)
         ("C-x C-u" . undo-redo)
         ("C-?"     . undo-redo)
         ("C-c w"   . fixup-whitespace)
         ("C-x C-d" . delete-pair)
         ("C-x O"   . other-other-window)
         ("M-c"     . capitalize-dwim)
         ("M-u"     . upcase-dwim)
         ("M-l"     . downcase-dwim)
         ("M-f"     . sim-vi-w)
         ("M-z"     . zap-up-to-char)
         ("C-x S"   . shell)
         ("C-x M-t" . transpose-regions)
         ("C-;"     . negative-argument)
         ("C-M-;"   . negative-argument)
         ("M-1" . delete-other-windows)
         ("M-2" . split-window-below)
         ("M-3" . split-window-right))
  :init
  (define-key key-translation-map (kbd "<mouse-4>") (kbd "<wheel-up>"))
  (define-key key-translation-map (kbd "<mouse-5>") (kbd "<wheel-down>"))
  (define-key key-translation-map (kbd "<mouse-6>") (kbd "<wheel-left>"))
  (define-key key-translation-map (kbd "<mouse-7>") (kbd "<wheel-right>"))
  (bind-key (kbd "<wheel-up>") #'previous-line)
  (bind-key (kbd "<wheel-down>") #'next-line)
  (bind-key (kbd "<wheel-left>") #'backward-char)
  (bind-key (kbd "<wheel-right>") #'forward-char)
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
  (when my/my-system
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
               :bold-weight extrabold)
              (t                        ; our shared fallback properties
               :default-family "PragmataPro Mono Liga"
               :default-weight normal)))
      (fontaine-set-preset 'regular))
    ;; Load pragmatapro-lig.el
    (load (concat user-emacs-directory
                  "lisp/pragmatapro-lig.el"))
    (require 'pragmatapro-lig)

    ;; Enable pragmatapro-lig-mode for specific modes
    (add-hook 'text-mode-hook 'pragmatapro-lig-mode)
    (add-hook 'prog-mode-hook 'pragmatapro-lig-mode))
;;;; Defaults
  ;; Handle long lines
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq-default bidi-inhibit-bpa t)
  (global-so-long-mode 1)

  (setq delete-by-moving-to-trash t
        create-lockfiles nil
        auto-save-default nil
        inhibit-startup-screen t
        ring-bell-function 'ignore)
;;;; Have focus follow the mouse rather than requiring a click
  (setq mouse-autoselect-window nil
        focus-follows-mouse nil)
;;;; UTF-8
  (prefer-coding-system 'utf-8)
;;;; Remove Extra Ui
  (fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"
  (show-paren-mode 1)              ; Highlight parenthesis
  (setq x-select-enable-primary t) ; use primary as clipboard in emacs
  ;; avoid leaving a gap between the frame and the screen
  (setq-default frame-resize-pixelwise t)

  ;; Vim like scrolling
  (setq scroll-step            1
        scroll-conservatively  10000)
  (setq next-screen-context-lines 5)
  ;; move by logical lines rather than visual lines (better for macros)
  (setq line-move-visual nil)
  (unless my/is-termux
    (fringe-mode))

  ;;TRAMP
  (setq tramp-default-method "ssh")
  (setq shell-file-name "bash")         ; don't use zsh
  ;; recentf
  (customize-set-value 'recentf-make-menu-items 150)
  (customize-set-value 'recentf-make-saved-items 150)

  ;; Unify Marks
  (customize-set-value 'global-mark-ring-max 256)
  (customize-set-value 'set-mark-command-repeat-pop 256)
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

;; Make ex mode avilable in emacs
(use-package viper
  :ensure nil
  :bind ("C-x ;" . viper-ex))

;;; General Key Bindings
(use-package crux
  :ensure t
  :bind (("C-x w v" . crux-swap-windows)
         ("C-S-o"   . crux-smart-open-line-above)
         ("C-o"     . crux-smart-open-line)
         ("C-x B"   . my/org-scratch))
  :config
  (defun my/org-scratch ()
    (interactive)
    (let ((initial-major-mode 'org-mode))
      (crux-create-scratch-buffer))))

(use-package simple
  :ensure nil
  :bind (("M-SPC" . cycle-spacing)))

(defun qalc (&optional arg)
  (interactive)
  (comint-run "qalc"))

(defun yall (&optional arg)
  (interactive)
  (comint-run
   (concat"/home/gavinok/.local/Dropbox/DropsyncFiles/vimwiki/School/SENG475/assignments/project/"
          "lisp")
   '("-i")))

;;; TERMINAL SETTINGS
(when my/is-terminal
  (progn (set-face-background 'default "undefinded")
         (add-to-list 'term-file-aliases
                      '("st-256color" . "xterm-256color"))
         (xterm-mouse-mode t)))

(when (require 'auto-mark nil t)
  (setq auto-mark-command-class-alist
        '((anything . anything)
          (goto-line . jump)
          (indent-for-tab-command . ignore)
          (undo . ignore)))
  (setq auto-mark-command-classifiers
        (list (lambda (command)
                (if (and (eq command 'self-insert-command)
                         (eq last-command-char ? ))
                    'ignore))))
  (global-auto-mark-mode 1))

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
  ;; :bind (:map vertico-map
  ;;             ("d"  . vertico-directory-delete-char)
  ;;             ("M-d" . vertico-directory-delete-word))
;;;; Out Of Order Compleiton
  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless flex)))

;;;; Extra Completion Functions
  (use-package consult
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
           ("M-s g"       . consult-grep)
           ("M-s M-g"     . consult-grep)
           ("C-x C-SPC"   . consult-global-mark)
           ("C-x M-:"     . consult-complex-command)
           ("C-c n"       . consult-org-agenda)
           ("C-c m"     . my/notegrep)
           :map dired-mode-map
           ("O" . consult-file-externally)
           :map help-map
           ("a" . consult-apropos)
           :map minibuffer-local-map
           ("M-r" . consult-history))
    :custom
    (completion-in-region-function #'consult-completion-in-region)
    :config
    (defun my/notegrep ()
      "Use interactive grepping to search my notes"
      (interactive)
      (consult-ripgrep org-directory))
    (add-hook 'completion-setup-hook #'hl-line-mode)
    (recentf-mode t))

  (load (concat user-emacs-directory
                "lisp/affe-config.el"))

  (use-package marginalia
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (vertico-mode t)
  :config
  ;; Used for the vertico-directory extension
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

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
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-d" . dragon-drop)
   ("U"   . 0x0-upload-file)
   :map embark-region-map
   ("U"   . 0x0-dwim))
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
(require 're-builder)
(defvar my/re-builder-positions nil
    "Store point and region bounds before calling re-builder")

(advice-add 're-builder
              :before
              (defun my/re-builder-save-state (&rest _)
                "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
                          (setq my/re-builder-positions
                                (cons (point)
                                      (when (region-active-p)
                                        (list (region-beginning)
                                              (region-end)))))))

(use-package re-builder
  :bind
  (("C-M-%" . re-builder)
   :map reb-mode-map
   ("RET" . reb-replace-regexp)
   :map reb-lisp-mode-map
   ("RET" . reb-replace-regexp))
  :init
  (setq reb-re-syntax #'rx)
  :config
  (defun reb-replace-regexp (&optional delimited)
    "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
    (interactive "P")
    (reb-update-regexp)
    (let* ((re (reb-target-binding reb-regexp))
           (replacement (query-replace-read-to
                         re
                         (concat "Query replace"
                                 (if current-prefix-arg
                                     (if (eq current-prefix-arg '-) " backward" " word")
                                   "")
                                 " regexp"
                                 (if (with-selected-window reb-target-window
                                       (region-active-p)) " in region" ""))
                         t))
           (pnt (car my/re-builder-positions))
           (beg (cadr my/re-builder-positions))
           (end (caddr my/re-builder-positions)))
      (with-selected-window reb-target-window
        (goto-char pnt) ; replace with (goto-char (match-beginning 0)) if you want
                                        ; to control where in the buffer the replacement starts
                                        ; with re-builder
        (setq my/re-builder-positions nil)
        (reb-quit)
        (query-replace-regexp re replacement delimited beg end)))))

;;;; Code Completion
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                  ; Allows cycling through candidates
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-prefix 2)            ; Enable auto completion
  (corfu-auto-delay 0.0)           ; Enable auto completion
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)   ; Enable auto completion
  (corfu-preview-current 'insert)   ; Do not preview current candidate
  (corfu-preselect-first nil)

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"     . nil) ;; leave my enter alone!
              )

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  :config
  (setq tab-always-indent 'complete)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))
;; Add extensions
(use-package cape
  :defer 10
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; Templates takes advantage of emacs's tempo
(use-package tempel
  :ensure t
  :defer 10
  :hook ((prog-mode text-mode) . tempel-setup-capf)
  :bind (("M-+" . tempel-insert) ;; Alternative tempel-expand
         :map tempel-map
         ([remap keyboard-escape-quit] . tempel-done)
         ("TAB" . tempel-next)
         ("<backtab>" . tempel-previous)
         :map corfu-map
         ("C-M-i" . tempel-expand))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions))))

;; For uploading files
(use-package 0x0
  :ensure t
  :commands (0x0-dwim 0x0-upload-file))

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
(use-package spaceway-theme
  :ensure nil
  :load-path "lisp/spaceway/"
  :config
  (global-hl-line-mode t)
  (set-cursor-color "#dc322f")
  ;; (when my/my-system
  ;;   (set-frame-parameter (selected-frame) 'alpha '(90 90))
  ;;   (add-to-list 'default-frame-alist '(alpha 90 90)))
  (load-theme 'spaceway t))

(global-hl-line-mode t)
(customize-set-value 'modus-themes-org-blocks 'gray-background
                     "Color background of code blocks gray.")
;; (load-theme 'modus-vivendi t)
(add-to-list 'default-frame-alist '(cursor-color . "magenta"))

;;; WRITING
(use-package writegood-mode
  :hook (flyspell-mode . writegood-mode))

(use-package writeroom-mode
  :commands (writeroom-mode global-writeroom-mode)
  :init
  (setq writeroom-width 90))

(use-package flyspell-correct
  :bind ("C-c DEL" . flyspell-correct-previous)
  :hook ((markdown-mode nroff-mode org-mode
                        mu4e-compose-mode
                        mail-mode
                        git-commit-mode)
         . flyspell-mode)
  :init
  (add-to-list 'ispell-skip-region-alist '("+begin_src" . "+end_src"))
  (setq flyspell-use-meta-tab nil))

;;; ORG
(load (concat user-emacs-directory
              "lisp/org-config.el"))

;;; Git
(use-package magit
  :bind ("C-x v SPC" . magit-status)
  :commands magit
  :config
  (use-package forge
    :after magit))

(use-package ediff
  :after (magit vc)
  :init
  ;; multiframe just doesn't make sense to me
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook 'winner-undo))
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl
  :unless my/is-termux
  :defer 5
  :init (global-diff-hl-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode))

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
;;; VTERM AND ESHELL
(use-package vterm
  :bind (("C-x t" . vterm)
         :map vterm-mode-map
         ("M-p" . vterm-send-up)
         ("M-n" . vterm-send-down))

  :commands vterm
  :custom (vterm-max-scrollback 10000)
  :init (when my/my-system
          (setq term-prompt-regexp ".*ᛋ")))

(use-package with-editor
  :hook ((shell-mode-hook eshell-mode-hook term-exec-hook vterm-exec-hook)
         . with-editor-export-editor)
  :bind (([remap async-shell-command] . with-editor-async-shell-command)
         ([remap shell-command] . with-editor-shell-command)))

(use-package em-alias
  :ensure nil
  :after eshell
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "e" "find-file $1")
              (eshell/alias "ee" "find-file-other-window $1")
              (eshell/alias "v" "view-file $1")
              (eshell/alias "o" "consult-file-externally $1"))))

(use-package em-term
  :ensure nil
  :after eshell
  :config
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-commands '("htop" "top" "git" "log" "diff" "show" "less")))

(use-package eshell
  :commands eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t))

;; More accureate color representation than ansi-color.el
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

;; interactive opening of files image preview and more from any repl
(use-package shx
  :ensure t
  :hook (shell-mode . shx-mode))

;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(use-package project
  :ensure nil
  :demand t
  :bind ("M-s M-s" . project-find-file)
  :config
  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  (defun project-magit  ()
    (interactive)
    (let ((dir (project-root (project-current t))))
      (magit-status dir)))

  (define-key project-prefix-map "m" 'project-magit)
  (define-key project-prefix-map "d" 'project-dired)
  (setq project-switch-commands
        '((project-find-file "Find file" f)
          (project-dired "Dired" d)
          (project-vc-dir "VC-Dir" v)
          (project-eshell "Eshell" e)
          (project-shell "Shell" s)
          (project-magit "Magit" m)))

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

  (add-to-list 'project-find-functions #'my/project-find-root)) ; [built-in] Project Managment

;;; COMPILATION
(use-package compile
  :defer t
  :bind (("C-x M-m" . compile)
         ("C-x C-m" . recompile))
  :config
  (setq compilation-scroll-output t)
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  ;; (setq compilation-environment '("HELLO=Hello"))
  )

;;; BUFFER MANAGMENT
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . hl-line-mode)
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)

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
              (ibuffer-switch-to-saved-filter-groups "home")))) ; [built-in] Powerful interface for managing buffers

;;; ISEARCH
(use-package isearch
  :ensure nil
  :bind (("C-s"     . isearch-forward)
         ("M-s M-%" . isearch-query-replace)
         ("C-r"     . isearch-backward)
         (:map isearch-mode-map
               ("\M-w" . isearch-save-and-exit)))
  :config
  (defun isearch-save-and-exit ()
    "Exit search normally. and save the `search-string' on kill-ring."
    (interactive)
    (isearch-done)
    (isearch-clean-overlays)
    (kill-new isearch-string))

  ;; Avoid typing - and _ during searches
  (setq search-whitespace-regexp "[-_ \t\n]+")

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
  (setq find-file-visit-truename t)
  ;; Save my spot when I jump to another file
  (advice-add 'ffap :before #'push-mark))

;;; Workspace Like Workflow
(use-package perspective
  :ensure t
  :init (setq persp-mode-prefix-key (kbd "C-x X"))
  :bind (("C-x X s" . persp-switch)
         ("C-x X C-l" . persp-state-load)
         ("C-x X B" . persp-switch-to-scratch-buffer))
  :config
  (persp-mode t))

;;; popup window managment
(use-package popper
  :ensure t ; or :straight t
  :bind (("M-`"     . popper-toggle-latest)
         ("M-~"     . popper-cycle)
         ("C-x M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
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
  (setq popper-display-control 'user)
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
                ("C-w" . yank))))

(load (concat user-emacs-directory
              "lisp/evil-config.el"))

(use-package mouse
  :ensure nil
  :defer 3
  :init
  (context-menu-mode 1))

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
(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-<tab>" . hs-cycle)
              ("C-c  z h" . hs-hide-all)
              ("C-c  z s" . hs-show-all))
  :init
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
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
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'font-lock-comment-face) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▾")))))

;; Automatic code formatting
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;;; LSP

;; Should boost performance with lsp
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "1")
(use-package lsp-mode
  :bind ((:map lsp-mode-map
               ("M-<return>" . lsp-execute-code-action))
         (:map c++-mode-map
               ("C-c x" . lsp-clangd-find-other-file))
         (:map c-mode-map
               ("C-c x" . lsp-clangd-find-other-file)))
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init
  ;; Increase the amount of data emacs reads from processes
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  (with-eval-after-load 'js
    (define-key js-mode-map (kbd "M-.") nil))
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-keymap-prefix "C-x L")
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless flex)))))))
:config
(use-package lsp-haskell :ensure t :hook (haskell-mode . lsp-deferred))
(use-package lsp-java :ensure t :hook (java-mode . lsp-deferred)
  :config
  (require 'lsp-java-boot)

  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode))
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :init
  (setq python-shell-enable-font-lock nil)
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))
(use-package lsp-ui
  :ensure t
  :after lsp
  :init
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-diagnostics t))

;;; Debugging
(use-package dap-mode
  :bind (:map dap-mode-map
              ("C-x D D" . dap-debug)
              ("C-x D d" . dap-debug-last))
  :after lsp-mode
  :init
  (defun my/dap-cpp-setup ()
    (require 'dap-gdb-lldb)
    (dap-gdb-lldb-setup))
  :config
  (my/dap-cpp-setup)
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

;;; Languages
(use-package nvm
  :ensure nil
  :commands (my/nvm-use)
  :quelpa (nvm :fetcher github :repo "rejeep/nvm.el")
  :commands (nvm-use nvm-use-for-buffer)
  :config
  (setq nvm-dir (concat (getenv "HOME") "/.config/nvm"))
  (defun my/nvm-use ()
    (interactive)
    (nvm-use (completing-read "Enter Node Version"
                              '("16.17.1")))))
;;;; Haskell
(use-package haskell-mode :ensure t :mode "\\.hs\\'"
  ;; lets you use C-c C-l
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-indent-mode))
(use-package flymake-hlint
  :hook (haskell-mode . flymake-hlint-load))

;;;; PureScript
(use-package purescript-mode :ensure t :mode "\\.purs\\'"
  :config
  (add-hook 'purescript-mode-hook 'purescript-indent-mode)
  ;; Setup auto formatting for purescript
  (push '(purs-tidy . ("purs-tidy format")) apheleia-formatters)
  (setf (alist-get 'purescript-mode apheleia-mode-alist) '(purs-tidy)))
(use-package psci
  :ensure t
  :after purescript-mode
  :config
  (add-hook 'purescript-mode-hook 'inferior-psci-mode))
;;;; Rust
(use-package rust-mode    :ensure t :mode "\\.rs\\'"
  :init
  ;; scratchpad for rust
  (setq lsp-rust-clippy-preference "on")
  (use-package rust-playground
    :commands (rust-playground)
    :ensure t)
  (setq rustic-lsp-client 'eglot))
;;;; Racket
(use-package racket-mode  :ensure t :mode "\\.rkt\\'"
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (add-hook 'racket-mode-hook 'prettify-symbols-mode)
  (defun setup-racket-eldoc ()
    (eldoc-mode +1)
    (setq eldoc-documentation-function #'racket-xp-eldoc-function))
  (add-hook 'racket-mode-hook #'setup-racket-eldoc))

;;; Clojure
(use-package clojure-mode :ensure t :mode "\\.clj\\'")

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
          (ccl ("ccl") :coding-system utf-8-unix)
          (ecl ("ecl") :coding-system utf-8-unix)
          (roswell ("ros" "-Q" "-l" ,ros-config "run"))
          (qlot ("qlot" "exec" "ros" "-l" ,ros-config "run" "-S" ".")
                :coding-system utf-8-unix))))

;;;; Setup Folding For Programming
(use-package elec-pair
  :ensure nil
  :init
  (electric-pair-mode t))
(use-package puni
  ;; :hook (;; use strict mode for programming languages and repls
  ;;        (prog-mode cider-repl-mode eshell-mode
  ;;                   fennel-repl-mode geiser-repl-mode inferior-emacs-lisp-mode
  ;;                   inferior-lisp-mode inferior-scheme-mode lisp-interaction-mode
  ;;                   racket-repl-mode scheme-interaction-mode sly-mrepl-mode)
  ;;        . puni-strict-mode)
  ;; (defun my/kill-sexp (&optional arg)
  ;;   (interactive)
  ;;   (progn (puni-expand-region)
  ;;          (kill-region arg)))
  :bind (("C-c s" . puni-mode)
         :map puni-mode-map
         ("C-c DEL" . flyspell-correct-previous)
         ("M-e"   . puni-end-of-sexp)
         ("M-a"   . puni-beginning-of-sexp)
         ;; ("C-M-u" . (lambda (&optional arg)
         ;;              (interactive)
         ;;              (up-list -1)))
         ("C-M-f" . forward-sexp)
         ("C-M-b" . backward-sexp)
         ("C-)"   . puni-slurp-forward)
         ("C-0"   . puni-slurp-forward)
         ("C-}"   . puni-barf-forward)
         ("C-9"   . puni-slurp-backward)
         ("C-{"   . puni-barf-backward)
         ("C-M-j" . sp-join-sexp)
         ("C-M-t" . puni-transpose)
         ("C-M-k" . kill-sexp)
         ("C-M-?" . puni-convolute)
         ("C-k"   . puni-kill-line)
         ("M-k"   . kill-sexp)
         ("M-k"   . kill-sexp)
         ("S-SPC" . puni-expand-region)
         ;; ("C-M-w" . sp-copy-sexp)
         ;; ("M-C"   . sp-clone-sexp)
         ("C-M-z" . puni-squeeze)
         ("M-<backspace>" . backward-kill-word)
         ("C-w" . my/kill-region))
  :init
  (puni-global-mode t)
  :config
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)

  ;;;; Better Killing And Yanking
  (setq rectangle-mark-mode nil)
  (setq *last-kill-was-rectangle* rectangle-mark-mode)

  (defun remember-last-kill-type (&rest d)
    (setq *last-kill-was-rectangle* rectangle-mark-mode))

  (advice-add 'kill-region :before #'remember-last-kill-type)
  (advice-add 'kill-ring-save :before #'remember-last-kill-type)
  (advice-add 'kill-rectangle :before #'remember-last-kill-type)

  (defun my/kill-region (BEG END &optional REGION)
    (interactive (list (mark) (point) 'region))
    (cond
     (rectangle-mark-mode (kill-rectangle
                           (region-beginning) (region-end)))
     (mark-active (kill-region
                   (region-beginning) (region-end)))
     (t (backward-kill-sexp 1))))

  (defun my/yank (&optional arg)
    (interactive)
    (if *last-kill-was-rectangle*
        (yank-rectangle)
      (yank arg)))

  (bind-key (kbd "C-y") 'my/yank)

  ;; Avoid terminal binding confilct
  ;; (unless my/is-termux
  ;;   (bind-key (kbd "M-[") #'puni-splice 'puni-mode-map)
  ;;   (bind-key (kbd "M-]") #'puni-split 'puni-mode-map))
  )

(use-package flymake
  :defer 10
  :bind (("M-g d"   . flymake-show-buffer-diagnostics)
         ("M-g M-d" . flymake-show-project-diagnostics)
         ("M-g M-n" . flymake-goto-next-error)
         ("M-g M-p" . flymake-goto-prev-error))
  :hook (prog-mode . (lambda () (flymake-mode t)))
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package outline
  :hook ((prog-mode tex-mode) . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              (("<backtab>" . outline-cycle-buffer)
               ("C-c u" . outline-up-heading)
               ("C-c j" . outline-forward-same-level)
               ("C-c k" . outline-backward-same-level)))
  :config
  ;; Outline Minor Mode
  (add-hook 'outline-minor-mode-hook
            (lambda () (local-set-key (kbd "C-t")
                                      outline-mode-prefix-map)))
  (defun set-vim-foldmarker (fmr)
    "Set Vim-type foldmarkers for the current buffer"
    (interactive "sSet local Vim foldmarker: ")
    (if (equal fmr "")
        (message "Abort")
      (setq fmr (regexp-quote fmr))
      (set (make-local-variable 'outline-regexp)
           (concat ".*" fmr "\\([0-9]+\\)"))
      (set (make-local-variable 'outline-level)
           `(lambda ()
              (save-excursion
                (save-match-data
                  (re-search-forward ,(concat fmr "\\([0-9]+\\)") nil t)
                  (string-to-number (match-string 1))))))))
  ;; Vim Like Folding
  (set-vim-foldmarker "{{{"))

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
         . global-paren-face-mode))

(use-package prog-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              (("C-c RET" . emacs-lisp-macroexpand)
               ("C-c C-k" . eval-buffer)))
  :init
  ;; Make all scripts executable. Ya this might be sketch but I don't
  ;; mind
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  ;; Don't prompt for a reference
  (setq xref-prompt-for-identifier nil)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (setq show-trailing-whitespace t)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-hook 'local-write-file-hooks 'check-parens)))
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'python-mode    'prettify-symbols-mode)
  (add-hook 'lisp-mode-hook #'(lambda () (setq indent-tabs-mode nil)))

;;;;; Smart Indentation
  (defun infer-indentation-style ()
    ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
    ;; neither, we use the current indent-tabs-mode
    (let ((space-count (how-many "^  " (point-min) (point-max)))
          (tab-count (how-many "^\t" (point-min) (point-max))))
      (if (> space-count tab-count) (setq indent-tabs-mode nil))
      (if (> tab-count space-count) (setq indent-tabs-mode t))))
  (add-hook 'prog-mode-hook 'infer-indentation-style))

;;; EXTRA UI
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package pulse
  ;; Highlight cursor postion after movement
  :unless my/is-terminal
  :defer t
  :init (defun pulse-line (&rest _)
          (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(other-window windmove-do-window-select mouse-set-point mouse-select-window))
    (advice-add command :after #'pulse-line)))

;;;; Display hex colors in emacs
(use-package rainbow-mode
  :defer t :commands (rainbow-mode))

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
        (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))

  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
;;;;; xdg-open integration
  (require 'dired-x)
  (setq dired-bind-jump nil)
  ;; prevent opening extra dired buffers
  ;; emacs 28
  (setq dired-kill-when-opening-new-dired-buffer t))
(use-package consult-dir
  :ensure t
  :bind (("C-x C-j" . consult-dir)
         ;; :map minibuffer-local-completion-map
         :map vertico-map
         ("C-x C-j" . consult-dir)))

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
(unless my/is-terminal
  (setq-default left-margin-width 2)
  (setq-default right-margin-width 2))
(set-window-buffer nil (current-buffer))

(defvar ml-text-scale-factor 1.0
  "Scale of mode-line font size to default font size, as a float.
This is needed to make sure that text is properly aligned.")

(defun ml-fill-to-center (reserve face)
  "Return empty space to the center, leaving RESERVE space on the right."
  (when ml-text-scale-factor
    (setq reserve (* ml-text-scale-factor reserve)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin))
                                             ,reserve
                                             (.5 . left-margin))))
              'face face))

(defun ml-fill-to-right (reserve face)
  "Return empty space, leaving RESERVE space on the right."
  (when ml-text-scale-factor
    (setq reserve (* ml-text-scale-factor reserve)))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 2))) ; Powerline uses 3 here, but my scrollbars are narrower.
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                             ,reserve)))
              'face face))

(defun ml-render-2-part (left right &optional fill-face)
  (concat left
          (ml-fill-to-right (string-width (format-mode-line right)) fill-face)
          right))


;; determin the focused window
(defvar cogent-line-selected-window (frame-selected-window))
(defun cogent-line-set-selected-window (&rest _args)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq cogent-line-selected-window (frame-selected-window))
    (force-mode-line-update)))
(defun cogent-line-unset-selected-window ()
  (setq cogent-line-selected-window nil)
  (force-mode-line-update))
(defun cogent-line-selected-window-active-p ()
  (eq cogent-line-selected-window (selected-window)))

(add-hook 'window-configuration-change-hook #'cogent-line-set-selected-window)
;; (add-hook 'focus-in-hook #'cogent-line-set-selected-window)
;; (add-hook 'focus-out-hook #'cogent-line-unset-selected-window)
;; (advice-add 'handle-switch-frame :after #'cogent-line-set-selected-window)
(add-hook 'window-selection-change-functions #'cogent-line-set-selected-window)

(setq-default mode-line-format
              `(" "
                ;; indicate if the buffer has been modified
                (:eval (propertize
                        (if (and (not buffer-read-only) (buffer-modified-p))
                            "● " "  " )
                        'face 'error))

                ;; Buffer name (no longer than 1/3 of the screen)
                mode-line-buffer-identification
                ;; (:eval (concat  (let ((bname (buffer-name (current-buffer)))
                ;;                       (max-len (/ (window-total-width) 3)))
                ;;                   (if (> (length bname) max-len)
                ;;                       (substring bname 0 max-len)
                ;;                     bname))))
                ;; value of current line number
                " "
                " %l:%c"
                (:eval (propertize
                        (concat " %p%"
                                " "
                                "「 %m 」")
                        'face (if (cogent-line-selected-window-active-p)
                                  'shadow
                                'mode-line-inactive)))

                ;; I still have yet to come up with a better option
                (:eval
                 (propertize " " 'display
	                     `((space :align-to
			              (- (+ right right-fringe right-margin)
			                 ,(string-width (format-mode-line mode-line-misc-info)))))))
                mode-line-misc-info))


(use-package time
  :defer 10
  :config
  (defface my/display-time
    '((((type x w32 mac))
       ;; #060525 is the background colour of my default face.
       (:foreground "#060525" :inherit bold))
      (((type tty))
       (:foreground "blue")))
    "Face used to display the time in the mode line.")
  (setq display-time-string-forms
        '((propertize (concat " " 12-hours ":" minutes " " am-pm " ")
                      'face 'my/display-time)))
  (set-face-attribute 'my/display-time nil :foreground "#fff" :background "#333"
                      :box '(:line-width 1 :color "#323"))
  (display-time-mode t))

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
  :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelabels)
  (setq-default pdf-view-display-size 'fit-page)
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link)))))

;;; mu4e
(load (concat user-emacs-directory
              "lisp/mu4e-config.el"))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

(use-package keycast
  :ensure t
  :commands (keycast-mode))

;; use emacs as a clipboard manager
(use-package clipmon
  :unless (and my/is-termux (not (executable-find "clipmon")))
  :defer 5
  :config
  (clipmon-mode-start))

;;; Winner Mode
(use-package winner
  :ensure nil
  :bind (("s-/" . winner-undo)
         ("s-?" . winner-redo))
  :config
  :init (winner-mode 1)) ; Window Managment Undo

(when my/my-system
  ;; Install `plz' HTTP library (not on MELPA yet).
  (use-package plz
    :ensure nil
    :quelpa (plz :fetcher github :repo "alphapapa/plz.el")
    :after ement)

  ;; Install Ement.
  (use-package ement
    :ensure nil
    :commands (my/ement-connect)
    :quelpa (ement :fetcher github :repo "alphapapa/ement.el")
    :init
    (customize-set-variable 'ement-room-message-format-spec "%B%r%R%t")
    (defun my/ement-connect ()
      (interactive)
      (ement-connect :user-id "@gavinok:matrix.org"
                     :password (password-store-get "riot.im/gavinok")))
    (set-fontset-font t 'unicode
                      "PragmataPro Mono:pixelsize=19:antialias=true:autohint=true" nil 'append))

  (use-package obs-websocket
    :ensure nil
    :quelpa (obs-websocket :fetcher github :repo "sachac/obs-websocket-el")
    :commands (obs-sel-scene)
    :config
    (use-package websocket :ensure t)
    (defun obs-sel-scene (&optional arg)
      (interactive)
      (unless obs-websocket
        (obs-websocket-connect))
      (let ((scene (completing-read "Select OBS Command"
                                    '("Intermission"
                                      "Desktop"))))
        (obs-websocket-send "SetCurrentScene" :scene-name scene))))

;;; Xournal support in org files
  (use-package org-xournalpp
    :ensure t
    :quelpa (org-xournalpp :fetcher gitlab :repo "vherrmann/org-xournalpp" :files ("*.el" "resources"))
    :bind ("C-c x" . org-xournalpp-insert-new-image)
    :config
    (add-hook 'org-mode-hook 'org-xournalpp-mode)))

(use-package evil-numbers
  :ensure t
  :bind (("C-x n a" . evil-numbers/inc-at-pt)
         ("C-x n x" . evil-numbers/dec-at-pt)))

(use-package repeat
  :defer 10
  :unless (version< emacs-version "28")
  :init
  (repeat-mode +1))

;; (use-package carp-mode
;;   :ensure nil
;;   :quelpa (carp-mode :fetcher github :repo "carp-lang/carp-emacs")
;;   :config
;;   (require 'carp-mode)
;;   (require 'inf-carp-mode)
;;   (require 'carp-flycheck)
;;   (add-hook 'carp-mode-hook
;;           (lambda ()
;;             (flycheck-mode 1))))

(use-package repeaters
  :ensure nil
  :quelpa (repeaters :fetcher github :repo "mmarshall540/repeaters")
  :config
  (repeaters-define-maps
   '(("Lines"
      next-line "C-n" "n"
      previous-line "C-p" "p")
     ("NAV"
      puni-forward-sexp "C-M-f" "f"
      puni-backward-sexp "C-M-b" "b"
      backward-up-list "C-M-u" "u"
      down-list "C-M-d" "d")
     ("Errors"
      flymake-goto-prev-error "p"
      flymake-goto-next-error "n")
     ("Nums"
      evil-numbers/inc-at-pt "a"
      evil-numbers/dec-at-pt "x")
     ("Org Nav"
      org-next-visible-heading "n"
      org-previous-visible-heading "p"
      org-cycle "<tab>"
      outline-up-heading "u"
      org-forward-heading-same-level "f"
      org-backward-heading-same-level "b"
      outline-down-heading "d"
      org-next-block "M-f")
     ("Outline Nav"
      outline-up-heading "C-c u" "u"
      outline-forward-same-level "C-c j" "j"
      outline-backward-same-level "C-c k" "k"))))

(use-package pomm
  :commands (pomm pomm-third-time)
  :config
  (pomm-mode-line-mode +1)
  (setq pomm-audio-enabled t)
  (setq pomm-work-period 20)
  (setq alert-default-style 'libnotify))

;;;; Use emacs instead of dmenu
(defun emenu (prompt options)
  "Create and select a frame called emacs-run-launcher which
consists only of a minibuffer and has specific dimensions. Run
the prompt given will be used and the user will be displayed a
dmenu like interface"
  (interactive)
  (let ((tmp-frame (make-frame '((name . "emacs-run-launcher")
				 (minibuffer . only)
				 (width . 120)
				 (height . 11)))))
    (with-selected-frame tmp-frame
      (unwind-protect
	  (completing-read prompt options)
        (delete-frame tmp-frame)))))

(setenv "LAUNCHER" "emenu -p ")
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
