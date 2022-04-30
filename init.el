;;; Startup
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold to speed up garbage collection
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

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
(setq package-native-compile t)
(setq comp-deferred-compilation t)

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
(defvar gv/is-termux
  (string-suffix-p
   "Android" (string-trim (shell-command-to-string "uname -a")))
  "Truthy value indicating if Emacs is currently running in termux.")
(defvar gv/is-terminal
  (not window-system)
  "Truthy value indicating if Emacs is currently running in a terminal.")
(defvar gv/my-system
  (if (string-equal user-login-name "gavinok")
      t
    nil)
  "Non-nil value if this is my system.")

(defun gv/scroll-down (arg)
  "Move cursor down half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (next-line dist)))

(defun gv/scroll-up (arg)
  "Move cursor up half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (previous-line dist)))

(global-set-key [remap scroll-up-command] #'gv/scroll-down)
(global-set-key [remap scroll-down-command] #'gv/scroll-up)

(defun gv/shell-command-on-file (command)
  "Execute COMMAND asynchronously on the current file."
  (interactive (list (read-shell-command
                      (concat "Async shell command on " (buffer-name) ": "))))
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (async-shell-command (concat command " " filename))))
(bind-key (kbd "C-M-&") #'gv/shell-command-on-file)
(with-eval-after-load 'image-mode
    (bind-key (kbd "&") #'gv/shell-command-on-file 'image-mode-map))

;;; General Key Bindings
(use-package crux
  :ensure t
  :bind (("C-x w v" . crux-swap-windows)
         ("C-S-o" . crux-smart-open-line-above)
         ("C-o" . crux-smart-open-line)))

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
  :init (global-undo-fu-session-mode))

;;; TERMINAL SETTINGS
(when gv/is-terminal
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
    :custom (completion-styles '(orderless flex)))

;;;; Extra Completion Functions
  (use-package consult
    :defer t
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
           ("C-x C-SPC"   . consult-global-mark)
           ("C-x M-:"     . consult-complex-command)
           ("C-c n"       . consult-org-agenda)
           ("C-c f"       . consult-find)
           ("C-c S-n"     . gv/notegrep)
           :map dired-mode-map
           ("O" . consult-file-externally)
           :map help-map
           ("a" . consult-apropos))
    :custom
    (completion-in-region-function #'consult-completion-in-region)
    :config
    (defun gv/notegrep ()
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
  (vertico-mode)
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
   ("C-d" . dragon-drop) ;; alternative for `describe-bindings'
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
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
                                 (concat "dragon-drag-and-drop " file))))
;;;; Code Completion
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)           ;; allows cycling through candidates
  (corfu-auto t)            ; Enable auto completion
  (corfu-auto-prefix 2)     ; Enable auto completion
  (corfu-auto-delay 0.0)    ; Enable auto completion
  (corfu-quit-at-boundary t)
  (corfu-echo-documentation 0.25)   ; Enable auto completion
  (corfu-preview-current 'insert)         ; Do not preview current candidate
  (corfu-preselect-first nil)

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("RET"     . nil) ;; leave my enter alone!
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous))

  :init
  (corfu-global-mode))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))

;; Templates takes advantage of emacs's tempo
(use-package tempel
  :ensure t
  :defer 10
  :hook ((prog-mode text-mode) . tempel-setup-capf)
  :bind (("M-+" . tempel-insert) ;; Alternative tempel-expand
         :map tempel-map
         ([remap keyboard-escape-quit] . tempel-done))
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
  (when gv/my-system
    (set-frame-parameter (selected-frame) 'alpha '(100 100))
    (add-to-list 'default-frame-alist '(alpha 100 100)))
  (load-theme 'spaceway t))

;;; WRITING
(defvar writting-modes '(markdown-mode nroff-mode org-mode
                                       mu4e-compose-mode
                                       mail-mode
                                       git-commit-mode))
(use-package writegood-mode
  :hook (flyspell-mode . writegood-mode))

(use-package flyspell-correct
  :bind ("C-c DEL" . flyspell-correct-previous)
  :hook (,writting-modes . turn-on-flyspell)
  :init (setq flyspell-use-meta-tab nil))

;;; ORG
(load (concat user-emacs-directory
              "lisp/org-config.el"))

;;; Git
(use-package magit
  :bind ("C-x v SPC" . magit-status)
  :commands magit)

(use-package ediff
  :after (magit vc)
  :init
  ;; multiframe just doesn't make sense to me
  (setq ediff-window-setup-function #'ediff-setup-windows-default)
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook 'winner-undo)))

(use-package diff-hl
  :unless gv/is-termux
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
  :bind ("C-x t" . vterm)
  :commands vterm
  :custom (vterm-max-scrollback 10000)
  :init (when gv/my-system
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

(use-package eshell
  :commands eshell
  :config
  (setenv "PAGER" "cat")
  (setq eshell-destroy-buffer-when-process-dies t))

(use-package em-term
  :ensure nil
  :after eshell
  :config
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-commands '("htop" "top" "git" "log" "diff" "show" "less")))

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

;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(use-package project
  :ensure nil
  :demand t
  :config
  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  (setq project-switch-commands
        '((project-find-file "Find file" f)
          (project-dired "Dired" d)
          (project-vc-dir "VC-Dir" v)
          (project-eshell "Eshell" e)
          (magit-status (project-root (project-current t)) "Magit" m)))

  (defvar project-root-markers
    '(".git" "package.clj" "package.json" "mix.exs" "Project.toml" ".project" "Cargo.toml" "qlfile"))

  (defun gv/project-find-root (path)
    (let* ((this-dir (file-name-as-directory (file-truename path)))
           (parent-dir (expand-file-name (concat this-dir "../")))
           (system-root-dir (expand-file-name "/")))
      (cond
       ((gv/project-root-p this-dir) (cons 'transient this-dir))
       ((equal system-root-dir this-dir) nil)
       (t (gv/project-find-root parent-dir)))))

  (defun gv/project-root-p (path)
    (let ((results (mapcar (lambda (marker)
                             (file-exists-p (concat path marker)))
                           project-root-markers)))
      (eval `(or ,@ results))))

  (add-to-list 'project-find-functions #'gv/project-find-root)) ; [built-in] Project Managment

;;; COMPILATION
(use-package compile
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
  :bind (("C-s"     . isearch-forward)
         ("M-s M-%" . isearch-query-replace)
         ("C-r"     . isearch-backward))
  :config
  (defun isearch-save-and-exit ()
    "Exit search normally. and save the `search-string' on kill-ring."
    (interactive)
    (isearch-done)
    (isearch-clean-overlays)
    (kill-new isearch-string))

  (define-key isearch-mode-map "\M-w" 'isearch-save-and-exit)

  ;; Avoid typing - and _ during searches
  (setq search-whitespace-regexp ".*")
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
         ("C-x x C-l" . persp-state-load)
         ("C-x x B" . persp-switch-to-scratch-buffer))
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
          "\\*eldoc\\*"
          "\\*compilation\\*"
          "^*tex"
          "\\*Ement Notifications\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Dtache Shell Command\\*"
          "\\*mu4e-update\\*"
          help-mode
          compilation-mode))
  (popper-mode +1))

(use-package evil
  :ensure t
  :commands (evil-mode)
  :bind (("<escape>" . keyboard-escape-quit)
         ("C-z" . evil-mode)
         :map evil-normal-state-map
         ;; vim vinigar style
         ("-"  . (lambda () (interactive)
                   (dired ".")))
         ("C-s" . consult-line)
         ;; Better lisp bindings
         ("(" . evil-previous-open-paren)
         (")" . evil-next-close-paren)
         ("<leader>/" . evil-ex-nohighlight)
         ("C-n" . evil-next-line)
         ("C-p" . evil-previous-line)
         :map evil-operator-state-map
         ("(" . evil-previous-open-paren)
         (")" . evil-previous-close-paren))
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-set-leader 'normal " "))

(use-package multiple-cursors
  :bind (("C-M-'" . mc/edit-lines)
         ("C-M-|" . mc/mark-all-in-region-regex)
         ("C-M-]" . mc/mark-next-like-this)
         ("C-M-[" . mc/mark-preveious-like-this)
         ("C-M-}" . mc/skip-to-next-like-this)
         ("C-M-{" . mc/skip-to-preveious-like-this)))

(defun my-change-number-at-point (change increment)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number increment)))
        (goto-char point)))))

(defun my-increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a"
  (interactive "p")
  (my-change-number-at-point '+ (or increment 1)))

(defun my-decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x"
  (interactive "p")
  (my-change-number-at-point '- (or increment 1)))

(global-set-key (kbd "C-x n a") 'my-increment-number-at-point)
(global-set-key (kbd "C-x n x") 'my-decrement-number-at-point)

;; Enable Commentary
(use-package evil-commentary
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)))

;; Enable Surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;; defaults
(use-package emacs
  :ensure nil
  :defer nil
  :bind (("C-c w"        . fixup-whitespace)
         ("C-x C-d"      . delete-pair)
         ("C-x O"        . other-other-window)
         ("M-c"          . capitalize-dwim)
         ("M-o"          . other-window)
         ("M-u"          . upcase-dwim)
         ("M-l"          . downcase-dwim)
         ("M-f"          . sim-vi-w)
         ("M-z"          . zap-up-to-char))
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
  (when gv/my-system
    (set-frame-font "PragmataPro Mono:pixelsize=22:antialias=true:autohint=true" nil t)
    ;; (set-frame-font "PragmataPro Mono:pixelsize=40:antialias=true:autohint=true" nil t)
    (load "~/.emacs.d/lisp/pragmatapro-lig.el")
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
  (menu-bar-mode -1) ; To disable the menu bar, place the following line in your .emacs file:
  (unless gv/is-termux
    (scroll-bar-mode -1)) ; To disable the scroll bar, use the following line:
  (tool-bar-mode -1) ; To disable the toolbar, use the following line:
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
  (unless gv/is-termux
    (fringe-mode))

  ;;TRAMP
  (setq tramp-default-method "ssh")
  (setq shell-file-name "bash")         ; don't use zsh
  ;; recentf
  (setq recentf-make-menu-items 150
        recentf-make-saved-items 150)

  ;; Unify Marks
  (setq global-mark-ring-max 256)
  (setq set-mark-command-repeat-pop 256)
  (defun push-mark (&optional location nomsg activate)
    "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil."
    (when (mark t)
      (let ((old (nth mark-ring-max mark-ring))
            (history-delete-duplicates nil))
        (add-to-history 'mark-ring (copy-marker (mark-marker)) mark-ring-max t)
        (when old
          (set-marker old nil))))
    (set-marker (mark-marker) (or location (point)) (current-buffer))
    (let ((old (nth global-mark-ring-max global-mark-ring))
          (history-delete-duplicates nil))
      (add-to-history
       'global-mark-ring (copy-marker (mark-marker)) global-mark-ring-max t)
      (when old
        (set-marker old nil)))
    (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
        (message "Mark set"))
    (if (or activate (not transient-mark-mode))
        (set-mark (mark t)))
    nil))

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
              ("C-c  C" . hs-toggle-hiding)))

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

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-h ." . display-local-help)
              ("C-h d" . eldoc-doc-buffer)
              ("M-RET" . eglot-code-actions))
  :ensure t
  :hook ((c-mode
          java-mode       ; M-x package-install eglot-java
          typescript-mode ; npm install -g typescript-language-server
          python-mode ; pip install --user 'python-language-server[all]' -U
          yaml-mode)  ; npm install -g yaml-language-server
         . eglot-ensure)
  :commands (eglot eglot-ensure)
  :init
  ;; go install github.com/mattn/efm-langserver@latest
  ;; Setup efm language server for writting modes since most don't
  ;; have a dedicated language server
  ;; (let ((modes `(,@writting-modes vimrc-mode shell-script-mode sh-mode))
  ;;       (efm (list "efm-langserver"
  ;;                  (concat "-c=" (getenv "HOME")
  ;;                          "/.emacs.d/efm/config.yaml"))))
  ;;   ;; Setup hooks
  ;;   (dolist (mode modes)
  ;;     (add-hook (intern (concat (symbol-name `,mode) "-hook"))
  ;;               'eglot-ensure))
  ;;   ;; Setup language server for the given modes
  ;;   (eval-after-load 'eglot
  ;;     (list 'add-to-list ''eglot-server-programs `(,modes . ,efm))))
  :config
  (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs
               '(yaml-mode . ("yaml-language-server" "--stdio"))))

(use-package haskell-mode :ensure t :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  ;; lets you use C-c C-l
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
(use-package flymake-hlint
  :hook
  (haskell-mode . flymake-hlint-load))
(use-package rust-mode    :ensure t :mode "\\.rs\\'"
  :init
  (setq rustic-lsp-client 'eglot))
(use-package racket-mode  :ensure t :mode "\\.rkt\\'")

;;; Clojure
(use-package clojure-mode :ensure t :mode "\\.clj\\'")
;; (use-package cider :ensure t :hook clojure-mode)

;;; Lisp
(use-package sly
  :commands (sly sly-connect)
  :init
  (setq inferior-lisp-program "sbcl")
  (setq sly-default-lisp 'roswell)
  (setq ros-config (concat user-emacs-directory
                                  "ros-conf.lisp"))
  (setq sly-lisp-implementations
        `((sbcl ("sbcl") :coding-system utf-8-unix)
          (ccl ("ccl") :coding-system utf-8-unix)
          (ecl ("ecl") :coding-system utf-8-unix)
          (roswell ("ros" "-Q" "-l" ,ros-config "run"))
          (qlot ("qlot" "exec" "ros" "-l" ,ros-config "run" "-S" ".")
                :coding-system utf-8-unix)))

  (defun qlot-sly ()
    "Start a sly repl using qlot at the projects root"
    (interactive)
    (let ((dir (cdr (project-current))))
      (if (cd dir)
          (sly 'qlot)
        (error (format "Failed to cd to %s" dir)))))

  (defun sly-critique-file ()
    "Lint this file with lisp-critic"
    (interactive)
    (sly-eval-async '(ql:quickload :lisp-critic))
    (sly-eval-async `(lisp-critic:critique-file ,(buffer-file-name))))

  (defun gv/connect-to-stumpwm ()
    (interactive)
    (start-process-shell-command "stumpish start-slynk" nil
                                 "stumpish start-slynk")
    (sly-connect "localhost" "4005")))

;;;; Setup Folding For Programming
(use-package smartparens
  :hook (;; use strict mode for programming languages and repls
         (prog-mode cider-repl-mode eshell-mode
                    fennel-repl-mode geiser-repl-mode inferior-emacs-lisp-mode
                    inferior-lisp-mode inferior-scheme-mode lisp-interaction-mode
                    racket-repl-mode scheme-interaction-mode sly-mrepl-mode)
         . smartparens-strict-mode)

  :bind (("C-c s" . smartparens-strict-mode)
         :map smartparens-mode-map
         ("M-e"   . sp-end-of-sexp)
         ("M-a"   . sp-beginning-of-sexp)
         ("M-("   . sp-wrap-round)
         ("C-M-d" . sp-down-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("C-)"   . sp-forward-slurp-sexp)
         ("C-}"   . sp-forward-barf-sexp)
         ("C-("   . sp-backward-slurp-sexp)
         ("C-{"   . sp-backward-barf-sexp)
         ("C-M-j" . sp-join-sexp)
         ("C-M-t" . sp-transpose-sexp)
         ("C-M-k" . sp-kill-sexp)
         ("C-M-?" . sp-convolute-sexp)
         ("C-k"   . sp-kill-hybrid-sexp)
         ("M-k"   . kill-sexp)
         ("C-M-SPC" . sp-mark-sexp)
         ("C-M-w" . sp-copy-sexp)
         ("M-C"   . sp-clone-sexp)
         ("C-M-z" . sp-rewrap-sexp)
         ("M-<backspace>" . backward-kill-word)
         ("C-<backspace>" . sp-backward-kill-word)
         ("C-c i" . sp-change-inner)
         ([remap sp-backward-kill-word] . backward-kill-word)
         ("C-w" . my/kill-region))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (setq rectangle-mark-mode nil)
  (defun my/kill-region (BEG END &optional REGION)
    (interactive (list (mark) (point) 'region))
    (cond
     (rectangle-mark-mode (kill-rectangle
                           (region-beginning) (region-end)))
     (mark-active (kill-region
                   (region-beginning) (region-end)))
     (t (sp-backward-kill-sexp 1))))

  ;; Avoid terminal binding confilct
  (unless gv/is-termux
    (bind-key (kbd "M-[") #'sp-splice-sexp 'smartparens-mode-map)
    (bind-key (kbd "M-]") #'sp-split-sexp 'smartparens-mode-map)))

(use-package flymake
  :defer 10
  :bind (("M-g d"   . flymake-show-buffer-diagnostics)
         ("M-g M-d" . flymake-show-project-diagnostics))
  :hook (prog-mode . (lambda () (flymake-mode t)))
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package outline
  :hook ((prog-mode tex-mode) . outline-minor-mode)
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
  :init
  (setq eldoc-echo-area-display-truncation-message nil)
  (global-eldoc-mode t))

(use-package prog-mode
  :ensure nil
  :config
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (setq show-trailing-whitespace t)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-hook 'local-write-file-hooks 'check-parens)))
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'lisp-mode-hook #'(lambda ()
                                (setq indent-tabs-mode nil)))
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
  :unless gv/is-terminal
  :defer t
  :init (defun pulse-line (&rest _)
          (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(other-window))
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

;;; MODELINE
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
                             " ( %m ) ") 'face 'shadow))
             mode-line-misc-info
             )
           ;; Right
           '((:eval (propertize
                     (format-time-string "%a, %b %d %I:%M%p")
                     'face 'font-lock-keyword-face))
             " "
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

;;; Winner Mode
(use-package winner
  :ensure nil
  :bind (("s-/" . winner-undo)
         ("s-?" . winner-redo))
  :config
  :init (winner-mode 1)) ; Window Managment Undo

;; Install and load `quelpa-use-package'.
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

(when gv/my-system
  ;; Install `plz' HTTP library (not on MELPA yet).
  (use-package plz
    :quelpa (plz :fetcher github :repo "alphapapa/plz.el")
    :after ement)

  ;; Install Ement.
  (use-package ement
    :commands (gv/ement-connect)
    :quelpa (ement :fetcher github :repo "alphapapa/ement.el")
    :init
    (setq ement-room-sender-headers t)
    (defun gv/ement-connect ()
      (interactive)
      (ement-connect :user-id "@gavinok:matrix.org"
                     :password (password-store-get "riot.im/gavinok")))
    ;; (setf use-default-font-for-symbols nil)
    ;; (set-fontset-font t 'unicode "Noto Emoji" nil 'append)
    )

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

(use-package eglot-java
  :ensure nil
  :quelpa (eglot-java :fetcher github :repo "yveszoundi/eglot-java")
  :after eglot
  :config
  (setq eglot-java-prefix-key "C-c e")
  (eglot-java-init)
  (defun setup-java ()
    (interactive)
    (setenv "JAVA_HOME" "/user/lib/jvm/java-8-openjdk-amd64/")
    (setenv "PATH"
            (concat
             "/user/lib/jvm/java-8-openjdk-amd64/bin:"
             (getenv "PATH")))))

;;;; Use emacs instead of dmenu
(setenv "LAUNCHER" "emenu -p ")
(setenv "EDITOR" "emacsclient")
;;; Stuff To Ignore
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(let ((f "lisp/termux.el"))
  (when (file-exists-p f)
    (load (concat user-emacs-directory f))))
