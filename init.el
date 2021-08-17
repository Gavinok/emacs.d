;;; Startup
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (expt 2 23))))
;;; PACKAGE LIST
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

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

;;;; Emacs look SIGNIFICANTLY less often which is a good thing.
;;;; asynchronous bytecode compilation and various other actions makes
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

(defun gv/read ()
  "text to speech"
  (interactive)
  (async-shell-command
   (concat "tts.sh '" (x-get-clipboard)"'")))

(defun gv/stars ()
  "prompt user on of my github stars or my repos"
  (interactive)
  (start-process-shell-command "Stars" nil "stars"))

;;;; Fix terminal escape
(when gv/is-terminal
  ;; For whatever reason terminals have trouble recognizing escape
  ;; form emacs properly. Found this solution at
  ;; https://github.com/emacsorphanage/god-mode/issues/43
  (defvar gv/fast-keyseq-timeout 200)

  (defun gv/-tty-ESC-filter (map)
    (if (and (equal (this-single-command-keys) [?\e])
	     (sit-for (/ gv/fast-keyseq-timeout 1000.0)))
	[escape] map))

  (defun gv/-lookup-key (map key)
    (catch 'found
      (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

  (defun gv/catch-tty-ESC ()
    "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
    (when (memq (terminal-live-p (frame-terminal)) '(t pc))
      (let ((esc-binding (gv/-lookup-key input-decode-map ?\e)))
	(define-key input-decode-map
	  [?\e] `(menu-item "" ,esc-binding :filter gv/-tty-ESC-filter)))))

  (gv/catch-tty-ESC))
;;; Modal Bindings
;; Vim style undo
(use-package undo-fu
  :init (global-unset-key (kbd "C-/"))
  :defer nil
  :bind (;; I hate it when I accidentl
         ("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))
(use-package undo-fu-session
  :after undo-fu
  :init
  (global-undo-fu-session-mode))

;; Can be used similar to vim's change list
(use-package goto-chg
  :bind (("C-c j" . goto-last-change)
	 ("C-c k" . goto-last-change-reverse)))

(use-package crux
  :bind (([remap beginning-of-line] . crux-move-beginning-of-line)
	 ;; Remove whitespace when killing at the end of a line
	 ([remap kill-line] . crux-kill-and-join-forward)
	 ("C-c C-e" . crux-eval-and-replace)
	 ;; Since C-j is so similar
	 ("C-S-o" . crux-smart-open-line-above)
	 ("C-o" . crux-smart-open-line)))

(use-package windmove
  :bind (("C-c v" . windmove-swap-states-left)))

;; Custome keymap for an active region
(use-package selected
  :hook
  (activate-mark . selected-global-mode)
  :bind (:map selected-keymap
	      ("<escape>" . keyboard-escape-quit)))

;;;; God mode keys everywhere
(use-package view
  :ensure nil
  :bind (("<escape>" . view-mode)
         :map view-mode-map
         ("i" . view-mode)
	 ("n" . next-line)
	 ("p" . previous-line)
         ("e" . move-end-of-line)
         ("a" . crux-move-beginning-of-line)
         ;; Vim like whitespace movement
         ("f" . forward-whitespace)
         ("b" . (lambda ()
                  (interactive)
                  (let ((current-prefix-arg -1)) ;; emulate C-u
                    (call-interactively 'forward-whitespace) ;; invoke align-regexp interactively
                    )))
         ("s" . consult-line)
         ("SPC"  . View-scroll-half-page-forward)
         ("S-SPC". View-scroll-half-page-backward)
         ("z" . view-rep-run))
  :init
  (setq view-read-only t)
    ;; Cursor shape
  (defun my-view-mode-update-cursor-type ()
    (setq cursor-type (if buffer-read-only 'box 'bar)))
  (add-hook 'post-command-hook #'my-view-mode-update-cursor-type)

  ;; My jank way to emulate vim's . functionality
  (advice-add 'view-mode :after 'view-rep)
  :config
  (defun view-rep (arg)
    "intelegently start and end recording macros"
    (if view-mode
        (kmacro-end-macro nil)
      (kmacro-start-macro nil)))

  (defun view-rep-run ()
    "run a previously defined macro either from view-mode or from insert mode"
    (interactive)
    (when view-mode
      (read-only-mode -1)
      (view-mode -1))
    (kmacro-end-and-call-macro nil)
    (view-mode 1)))


(use-package simple
  :ensure nil
  :bind (("M-SPC" . cycle-spacing)))


(use-package multiple-cursors
  :bind (("C-x <" . mc/mark-previous-like-this)
	 ("C-x >" . mc/mark-next-like-this)
	 ("C-x C->" . mc/mark-all-like-this)))
;;; Extending Selection
;; Expand region is useful for adding iw aw style selection to emacs
;; TODO find a better alternative so I can put this in usepackage
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;;; TERMINAL SETTINGS
(when gv/is-terminal
    (progn (set-face-background 'default "undefinded")
	 (add-to-list 'term-file-aliases
		      '("st-256color" . "xterm-256color"))
	 (xterm-mouse-mode t))
	(global-set-key (kbd "<mouse-4>") 'next-line)
	(global-set-key (kbd "<mouse-5>") 'previous-line))

;;; COMPLETION
(use-package vertico
  :init
  (use-package marginalia
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))
;;;; Out Of Order Compleiton
  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless)))
;;;; Extra Completion Functions
  ;; Note that M-sn is used for searghing ang
  (use-package consult
    :bind (("C-c l"     . consult-line)
	   ("C-c i"     . consult-imenu)
	   ("C-c o"     . consult-outline)
	   ("C-x b"     . consult-buffer)
	   ("C-x C-k C-k" . consult-kmacro)
           ("C-c n" . gv/notegrep))
    :custom
    (completion-in-region-function #'consult-completion-in-region)
    :config
    (add-hook 'completion-setup-hook #'hl-line-mode)
    (defun gv/notegrep ()
      "Use interactive grepping to search my notes"
      (interactive)
      (consult-ripgrep org-directory)))
;;;; Fuzzy Finding
  (use-package affe
    :bind (("C-c f" . affe-find)
	   ("C-c g" . affe-grep))
    :commands (affe-grep affe-find)
    :config
    ;; only exclude git files
    (setq affe-find-command
	  (concat "find  "
		  "-not -path '*/\\.nnn*' -not -path '*/\\.git*' "
		  "-type f"))
    (defun affe-orderless-regexp-compiler (input _type)
      (setq input (orderless-pattern-compiler input))
      (cons input (lambda (str) (orderless--highlight input str))))
    (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
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

;;; THEMEING
(use-package ujelly-theme
  :ensure nil
  :config
  (load-theme 'ujelly t)
  (set-frame-parameter (selected-frame) 'alpha '(90 90))
  (add-to-list 'default-frame-alist '(alpha 90 90))
  (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
  ;; (set-face-attribute 'mode-line nil :foreground "#bdc3ce" :background "#000")
  (set-face-attribute 'default nil :background "#000" :foreground "#eee")
)

;;; Aligning Text
(use-package align
  :ensure nil
  :bind ("C-x C-a" . align-regexp)
  :config
  ;; Align using spaces
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))

;;; WRITING
(use-package writegood-mode
  :hook (flyspell-mode . writegood-mode))
(use-package flyspell-correct
  :bind ("C-c DEL" . flyspell-correct-previous)
  :hook ((org-mode mu4e-compose-mode mail-mode git-commit-mode)
	 . turn-on-flyspell))

(use-package academic-phrases
  :commands (academic-phrases))

;;; ORG
(if gv/is-termux
    (setq org-directory "~/storage/shared/Dropbox/Documents/org")
  (setq org-directory "~/Documents/org"))
(use-package org
  :pin org
  :ensure org-plus-contrib
  :commands (org-capture org-agenda)
  :bind (("C-c y" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :config
  (add-hook 'org-mode-hook (lambda () (setq indent-tabs-mode nil)))
;;;; Archive Completed Tasks
  (defun my-org-archive-done-tasks ()
    (interactive)
      (org-map-entries 'org-archive-subtree "/DONE" 'file)
      (org-map-entries 'org-archive-subtree "/CANCELLED" 'file))
;;;; Better defaults
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-special-ctrl-a/e t
	org-src-fontify-natively t
	org-fontify-quote-and-verse-blocks t
	org-src-tab-acts-natively t
	org-edit-src-content-indentation 2
	org-hide-block-startup nil
	org-src-preserve-indentation nil
	org-startup-folded 'content
	org-cycle-separator-lines 2)

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)"
		    "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
		    "|" "DELEGATED(D)" "CANCELLED(c)")))

;;;; Agenda Views
  (setq org-agenda-custom-commands
	'(("d" "Today's Tasks"
	   ((agenda "" ((org-agenda-span 1)
			(org-agenda-overriding-header "Today's Tasks")))))
	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))))
;;;; Capture
  (setq org-default-notes-file (concat org-directory "/refile.org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file (lambda () (concat org-directory "/refile.org")))
	   "* TODO %?\nDEADLINE: %T\n  %a")
	  ("M" "movie" entry (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
	   "* Meeting with  %?\nSCHEDULED: %T\n")
	  ("m" "Meeting" entry (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
	   "* Meeting with  %?\nSCHEDULED: %T\n")
	  ("r" "Refund" entry (file+olp (lambda () (concat org-directory "/Work.org"))
					"Work" "Refunds")
	   "* TODO Refund %?\n%?  %a\n")
	  ("w" "Waitlist" entry (file+olp (lambda () (concat org-directory "/Work.org"))
					  "Work" "Waitlist")
	   "* %?\n%? %a\n")
	  ("v" "Video Idea" entry (file+olp (lambda () (concat org-directory "/youtube.org"))
					    "YouTube" "Video Ideas")
	   "* %?\n%? %a\n")
	  ("c" "Cool Thing" entry (file+opl+datetree (lambda () (concat org-directory "/archive.org")))
	   "* %?\nEntered on %U\n  %i\n  %a")))
;;;; Refile targets
  (setq org-refile-targets
	'(("Work.org"    :maxlevel . 3)
	  ("archive.org" :maxlevel . 3)
	  ("mylife.org"  :maxlevel . 3)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
;;;; Font Sizes
  (dolist (face '((org-level-1 . 1.05)
		  (org-level-2 . 1.05)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.05)))
    (set-face-attribute (car face) nil :font "RobotoMono Nerd Font" :weight 'medium :height (cdr face))))


;;;; Drag And Drop
(use-package org-download
  :unless gv/is-terminal
  :bind ("s-i" . org-download-screenshot)
  :hook ((org-mode dired-mode) . org-download-enable)
  :init
  (setq org-agenda-files (seq-filter (lambda (x) (not (string-match "completed.org" x)))
				     (directory-files-recursively org-directory "\\.org$")))
  (setq-default org-download-screenshot-method "gnome-screenshot -a -f %s")
  (setq-default org-download-image-dir "./pic"))
;;;; Better Looking Bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;;;; Templates
(use-package org-tempo
  :ensure nil
  :defer t
  :config
  (add-to-list 'org-structure-template-alist '("sh"  . "src sh"))
  (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("vim"  . "src vim")))

;;;; Indentation
(use-package org-indent
  :ensure nil
  :defer t)

;;; Git
(use-package magit
  :bind ("C-x g" . magit-status)
  :commands magit)

(use-package diff-hl
  :unless gv/is-termux
  :defer 3
  :init (global-diff-hl-mode))
;;; As You Type Completion
(use-package company
  :defer 3
  :config
  (global-company-mode nil)
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1))

;;; VTERM AND ESHELL
(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000))

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

;;; LANGS
(use-package fennel-mode
  :mode "\\.fnl\\'")
(use-package racket-mode
  :mode "\\.rkt\\'")

;;; TREE-SITTER
;; Tree-sitter support
(use-package tree-sitter
  :commands (tree-sitter-mode))
;; install the tree-sitter grammars
(use-package tree-sitter-langs
  :after tree-sitter)

;;; LSP
(use-package eglot
  :commands eglot
  :hook ((java-mode c-mode-common) . eglot-ensure)
  :config
  (defconst my/eclipse-jdt-home "/usr/share/java/jdtls/plugins/org.eclipse.equinox.launcher.gtk.linux.x86_64_1.2.200.v20210406-1409.jar")

(defun my/eclipse-jdt-contact (interactive)
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" my/eclipse-jdt-home))
    (unwind-protect
	(eglot--eclipse-jdt-contact nil)
      (setenv "CLASSPATH" cp))))

(setcdr (assq 'java-mode eglot-server-programs) #'my/eclipse-jdt-contact)
  (add-to-list 'eglot-server-programs '(c-mode . ("ccls")))
  (add-to-list 'eglot-server-programs '(java-mode . ("jdtls"))))

;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(use-package project
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :ensure nil
  :defer t
  :config
  (defun my-git-project-finder (dir)
    "Integrate .git project roots."
    (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
		       (expand-file-name dir))))
      (and dotgit
	   (cons 'transient (file-name-directory dotgit)))))
  (add-hook 'project-find-functions 'my-git-project-finder)) ; [built-in] Project Managment
;;; COMPILATION
(use-package compile
  :bind ("C-x C-m" . compile))

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
	 ("Browser" (mode . eaf-mode)))))
  (add-hook 'ibuffer-mode-hook
	  (lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))) ; [built-in] Powerful interface for managing buffers

;;; ISEARCH
(use-package isearch
  :ensure nil
  :commands (isearch-forward isearch-backward)
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
;;; DEFAULTS
(use-package emacs
  :ensure nil
  :defer nil
  :bind (("C-x C-d"    . delete-pair)
         ("M-?" . help-command)
         ("C-h" . delete-backward-char)
         ("C-x f" . ffap))
  :config
  (hl-line-mode t)
  ;; No delay when deleting pairs
  (setq delete-pair-blink-delay 0)

  ;; change truncation indicators
  (define-fringe-bitmap 'right-curly-arrow
    [#b10000000
     #b10000000
     #b10000000
     #b01000000
     #b01000000
     #b01000000
     #b00100000
     #b00100000
     #b00100000
     #b00010000
     #b00010000
     #b00010000])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00000000])
  (set-frame-font "RobotoMono Nerd Font 14" nil t)
  ;; Replace selection on insert
  (delete-selection-mode 1)
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
  (setq line-move-visual nil))

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
  :defer nil
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
	      ("<backtab>" . outline-cycle-buffer))
  :init
  (setq outline-minor-mode-prefix "\C-c"))

;;;; Setup Folding For Programming
(use-package prog-mode
  :ensure nil
  :config
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

;;; EXTRA UI
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
  :defer nil
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

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;;; EMAIL
(use-package mu4e
  :unless gv/is-termux
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :ensure nil
  :bind ("C-x m" . mu4e-compose-new)
  :commands (mu4e mu4e-compose-new)
  :init
  ;; set *before* loading mu4e; and restart emacs if you want to change it
  ;; users of use-packag~ should can use the :init section for this.
  ;; (setq mu4e-view-use-old t)
  ;; Show full email address
  (setq mu4e-view-show-addresses 't
	mu4e-maildir "~/.local/share/mail"
	;; where to put attachemnts
	mu4e-attachment-dir  "~/Downloads"
	;; use mu4e for e-mail in emacs
	mail-user-agent 'mu4e-user-agent)
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; This prevents saving the email to the Sent folder since gmail will do this for us on their end.
  (setq mu4e-sent-messages-behavior 'delete
	message-kill-buffer-on-exit t)

;;;; Attachment reminders
  (use-package message-attachment-reminder)
;;;; Org In Emails
  (use-package org-mime)
;;;; mbsync and msmtp setup
  (setq mu4e-get-mail-command "mailsync"
	sendmail-program "/usr/bin/msmtp"
	message-sendmail-extra-arguments '("--read-envelope-from")
	send-mail-function 'smtpmail-send-it
	message-sendmail-f-is-evil t
	message-send-mail-function 'message-send-mail-with-sendmail)

  ;;images in emails
  (setq mu4e-view-show-images t)
;;;; Abbreviations
  (define-abbrev-table 'mu4e-compose-mode-abbrev-table
    '(("sin" "Sincerely, \nGavin Jaeger-Freeborn" nil 1)
      ("fwn_board" "<lammerdawn@gmail.com>, <president@flatwaternorth.ca>, <coach@flatwaternorth.ca>, <lewis_ds@me.com>, <a.kporter@klondiker.com>, <nikibparry@gmail.com>" nil 1)
      ("fwn_staff" "doonmcdowell51@gmail.com, makenna.macauley@gmail.com, jmcdowell1@live.ca, mountain2adventure@gmail.com, ballet@mkcd.ca, roganparry@gmail.com,joelgirouard23@gmail.com,brucep205@gmail.com, marin.ruth@icloud.com, sneill@northwestel.net,james.mccann.2006@gmail.com,torimarieski8@icloud.com, kalebparry1@gmail.com" nil 1)
      ("fwn_sprint" "<helenannegirouard@icloud.com>, <lammerdawn@gmail.com>, <president@flatwaternorth.ca>, <coach@flatwaternorth.ca>, <lewis_ds@me.com>, <a.kporter@klondiker.com>, <nikibparry@gmail.com>, doonmcdowell51@gmail.com, makenna.macauley@gmail.com, jmcdowell1@live.ca, mountain2adventure@gmail.com, ballet@mkcd.ca, roganparry@gmail.com,joelgirouard23@gmail.com,brucep205@gmail.com, marin.ruth@icloud.com, sneill@northwestel.net,james.mccann.2006@gmail.com,torimarieski8@icloud.com, kalebparry1@gmail.com" nil 1)
      ("fwn_roadshow" "g.gilbert.g@gmail.com, lpvidal@hotmail.com" nil 1)))

  (add-hook 'mu4e-compose-mode-hook #'abbrev-mode)
  (add-to-list 'auto-mode-alist '("^/tmp/neomutt.*\\'" . mail-mode)) ;; neomutt
  (add-to-list 'auto-mode-alist '(".*snd\.[0-9].*\\'" . mail-mode))  ;; mblaze
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)

;;;; Accounts
  (setq mu4e-contexts
	(list
	 ;; Personal account
	 (make-mu4e-context
	  :name "Personal"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address                . "gavinfreeborn@gmail.com")
		  (user-full-name                   . "Gavin Jaeger-Freeborn")
		  (mu4e-drafts-folder               . "/personal/[Gmail].Drafts")
		  (mu4e-sent-folder                 . "/personal/[Gmail].Sent Mail")
		  (mu4e-refile-folder               . "/personal/[Gmail].All Mail")
		  (mu4e-trash-folder                . "/personal/[Gmail].Trash")))

	 ;; Info Work account
	 (make-mu4e-context
	  :name "Info"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/info" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address                . "info@flatwaternorth.ca")
		  (user-full-name                   . "Gavin Jaeger-Freeborn")
		  (mu4e-drafts-folder               . "/info/[Gmail].Drafts")
		  (mu4e-sent-folder                 . "/info/[Gmail].Sent Mail")
		  (mu4e-refile-folder               . "/info/[Gmail].All Mail")
		  (mu4e-trash-folder                . "/info/[Gmail].Trash")))

	 ;; Coach Work account
	 (make-mu4e-context
	  :name "Coach"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/coach" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address                . "coach@flatwaternorth.ca")
		  (user-full-name                   . "Gavin Jaeger-Freeborn")
		  (mu4e-drafts-folder               . "/coach/[Gmail].Drafts")
		  (mu4e-sent-folder                 . "/coach/[Gmail].Sent Mail")
		  (mu4e-refile-folder               . "/coach/[Gmail].All Mail")
		  (mu4e-trash-folder                . "/coach/[Gmail].Trash")))))
;;;; Contacts
  (setq mail-personal-alias-file  "~/.config/mutt/aliases")
  (setq mu4e-org-contacts-file  (concat org-directory "/contacts.org"))
  (add-to-list 'mu4e-headers-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t)
;;;; Bookmarks
  (setq mu4e-bookmarks '((:name "To Handle"
				:query "((flag:flagged AND (NOT flag:replied)) OR (NOT flag:seen))" :key 116)
			 (:name "Today's messages"
				:query "date:today..now" :key 118)
			 (:name "Last 7 days"
				:query "date:7d..now" :hide-unread t :key 119)
			 (:name "Messages with images"
				:query "mime:image/*" :key 112))) ; email client depends on mu command

;;;; Headers View
  (set-face-attribute 'mu4e-flagged-face nil
		      :background "#900")
  (set-face-attribute 'mu4e-flagged-face nil
		      :foreground "#000")
  (set-face-attribute 'mu4e-unread-face nil
		      :background "#900")
  (set-face-attribute 'mu4e-unread-face nil
		      :foreground "#000")
  (setq-default mu4e-use-fancy-chars t)
  (setq-default mu4e-header-sort-field :date)
  (setq-default mu4e-headers-show-threads nil)
  (setq-default mu4e-headers-fields '((:flags         .    6)
				      (:from-or-to    .   22)
				      (:subject       .   70)
				      (:human-date    .   nil)))
  (mu4e-hide-other-mu4e-buffers))
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
;;; Winner Mode
(use-package winner
  :ensure nil
  :bind (( "s-/" . winner-undo)
	 ( "s-?" . winner-redo))
  :config
  :init (winner-mode 1)) ; Window Managment Undo

;;; MODELINE
(setq-default header-line-format '(" "))
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
                     (if (buffer-modified-p) "● " "  " ) 'face 'error))
	     mode-line-buffer-identification
	     ;; value of current line number
	     " %l:%c"
	     (:eval (propertize
                     (concat " %p%%" " "
                             (if god-local-mode " 😇 " "  ")
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

;;; Extras
;; (use-package quelpa-use-package)
;; Don't forget to run M-x eaf-install-dependencies
(use-package eaf
  :unless gv/is-termux
  :bind (("C-c w" . gv/bm)
	 ("s-w" . gv/bm))
  :defer t
  ;; :quelpa (eaf :fetcher github
  ;;             :repo  "manateelazycat/emacs-application-framework"
  ;;             :files ("*"))
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :init
  (use-package epc      :defer t :ensure t)
  (use-package ctable   :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s        :defer t :ensure t)
  (setq browse-url-browser-function 'eaf-open-browser)
  (defalias 'browse-web #'eaf-open-browser)
  :config
  (defun slurp (f)
    (with-temp-buffer
      (insert-file-contents f)
      (buffer-substring-no-properties
       (point-min)
       (point-max))))
  (defun gv/bm ()
    (interactive)
    (let ((selected (completing-read
		     "Select URL: " (split-string
				     (slurp "~/.config/bookmarks") "\n" t))))
      (let ((url (car (split-string
		       selected
		       " " t))))
	(if (string-match-p "\\http.*\\'" url)
	    ;; Open selected url
	    (eaf-open-browser url)
	  ;; Search entered text
	  (eaf-search-it selected)))))
  (setq-default eaf-browser-continue-where-left-off t)
  (setq-default eaf-browser-dnefault-search-engine "duckduckgo")
  (setq-default eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (dolist (keys '("i" "h" "j" "k" "l"
		  "d" "f" "x" ","
		  "-" "." "0" "1"
		  "2" "=" "B" "F"
		  "G" "H" "I" "J"
		  "K" "L" "P" "T"
		  "Y" "c" "d" "e"
		  "f" "g" "m" "n"
		  "o" "p" "r" "t"
		  "u" "v" "x" "y"
		  (eaf-bind-key nil key eaf-browser-keybinding)))))

;;;; Better PDFs
;https://github.com/politza/pdf-tools
; annotate pdfs with c-c c-a
; hl with c-c c-a h
; for help M-x pdf-tools-help RET
(use-package pdf-tools
  :mode "\\.pdf\\'"
  :config
  ;; pdf auto refresh
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

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

;;;; Use emacs instead of dmenu
(setenv "LAUNCHER" "emenu -p ")
;;;; Stuff To Ignore
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/gavinok/Documents/org/2021_canoekids_waitlist.org" "/home/gavinok/Documents/org/2021_flatwaternorth_executive_meeting.org" "/home/gavinok/Documents/org/2021_june_flatwaternorth_executive_meeting.org" "/home/gavinok/Documents/org/2021_may_flatwaternorth_executive_meeting.org" "/home/gavinok/Documents/org/Athletes.org" "/home/gavinok/Documents/org/Practices.org" "/home/gavinok/Documents/org/Work.org" "/home/gavinok/Documents/org/archive.org" "/home/gavinok/Documents/org/backlog.org" "/home/gavinok/Documents/org/c.org" "/home/gavinok/Documents/org/contacts.org" "/home/gavinok/Documents/org/divisionals_meeting.org" "/home/gavinok/Documents/org/elm.org" "/home/gavinok/Documents/org/fennel.org" "/home/gavinok/Documents/org/guile.org" "/home/gavinok/Documents/org/hy.org" "/home/gavinok/Documents/org/interviews_flatwaternorth.org" "/home/gavinok/Documents/org/janet.org" "/home/gavinok/Documents/org/joker.org" "/home/gavinok/Documents/org/june_ptso.org" "/home/gavinok/Documents/org/mylife.org" "/home/gavinok/Documents/org/nationals_prep.org" "/home/gavinok/Documents/org/parents_meeting.org" "/home/gavinok/Documents/org/racket.org" "/home/gavinok/Documents/org/rec.org" "/home/gavinok/Documents/org/refile.org" "/home/gavinok/Documents/org/reminders.org" "/home/gavinok/Documents/org/results.org" "/home/gavinok/Documents/org/roadshow.org" "/home/gavinok/Documents/org/staff_orientation.org" "/home/gavinok/Documents/org/staff_orientation_day.org" "/home/gavinok/Documents/org/today.org" "/home/gavinok/Documents/org/youtube.org" "/home/gavinok/Documents/org/yukon_river_quest.org"))
 '(package-selected-packages
   '(hl-todo rainbow-delimiters modus-operandi-theme all-the-icons-dired highlight-indent-guides typing-game c-c-combo corfu xah-fly-keys academic-phrases selected system-packages goto-chg writegood-mode which-key vterm vlf vimrc-mode vertico undo-fu-session undo-fu ujelly-theme tree-sitter-langs transmission rainbow-mode racket-mode quelpa-use-package pdf-tools pcre2el password-store outline-minor-faces org-superstar org-roam org-plus-contrib org-mime org-download org-alert orderless multiple-cursors modus-themes message-attachment-reminder marginalia magit lua-mode keycast jumplist god-mode flyspell-correct fish-completion fennel-mode expand-region esh-autosuggest epc eglot eaf diff-hl dashboard crux bicycle beacon all-the-icons affe)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
