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

;;; Startup
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))
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
(setq gv/is-termux
      (string-suffix-p
       "Android" (string-trim (shell-command-to-string "uname -a"))))

(defun gv/read ()
  "text to speech"
  (interactive)
  (async-shell-command
   (concat "tts.sh '" (x-get-clipboard)"'")))

(defun sudo-save ()
  "save this file as super user"
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; EVIL MODE
;; Vim style undo
(use-package undo-fu
  :init (global-unset-key (kbd "C-/"))
  :bind (( "C-/" . undo-fu-only-undo)
	 ("C-?" . undo-fu-only-redo)))
(use-package undo-fu-session
  :after undo-fu
  :init
  (global-undo-fu-session-mode))

(use-package  evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal " ")
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (evil-define-key 'normal   'global (kbd "<leader>t") 'capitalize-dwim)
  (evil-define-key 'visual   'global (kbd "<leader>t") 'capitalize-dwim)
  (evil-define-key 'insert   'global (kbd "M-n")       'hippie-expand)
  ;; Better lisp bindings
  (evil-define-key 'normal   'global (kbd "(")         'evil-previous-open-paren)
  (evil-define-key 'normal   'global (kbd ")")         'evil-next-close-paren)
  (evil-define-key 'operator 'global (kbd "(")         'evil-previous-open-paren)
  (evil-define-key 'operator 'global (kbd ")")         'evil-previous-close-paren)
;;;;; vim vinigar style
  (define-key  evil-normal-state-map (kbd "-") (lambda () (interactive)
						 (dired ".")))
  )

(use-package evil-collection
  :config
  (evil-collection-init)
  ;; Dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "-" 'dired-up-directory))

;; Enable Commentary
(use-package evil-commentary
  :config
  (evil-commentary-mode 1))

;; Enable Surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Enable Lion
(use-package evil-lion
  :config
  (evil-lion-mode 1)
  (evil-define-key 'normal 'global (kbd "gl") 'evil-lion-left)
  (evil-define-key 'normal 'global (kbd "gL") 'evil-lion-right))

;; Cursor Shape
(use-package evil-terminal-cursor-changer
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))

;;; TERMINAL SETTINGS
(if (display-graphic-p)
    (set-face-background 'default "#000000")
  (progn (set-face-background 'default "undefinded")
	 (add-to-list 'term-file-aliases
		      '("st-256color" . "xterm-256color"))
	 (xterm-mouse-mode t))
	(global-set-key (kbd "<mouse-4>") 'next-line)
	(global-set-key (kbd "<mouse-5>") 'previous-line))

;;; COMPLETION
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-c C-s" . consult-line)
	 ("C-M-l" . consult-imenu)
	 ("C-M-j" . persp-switch-to-buffer*)
	 :map minibuffer-local-map)
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (add-hook 'completion-setup-hook #'hl-line-mode))
(use-package affe
  :after orderless
  :bind (("C-c o" . gv/notegrep)
	 ("C-c f" . affe-find)
	 ("C-c g" . affe-grep))
  :commands (affe-grep affe-find)
  :config
  ;; only exclude git files
  (setq affe-find-command "find  -not -path '*/\\.nnn*' -not -path '*/\\.git*' -type f")
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
	affe-highlight-function #'orderless--highlight)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  (defun gv/notegrep ()
    (interactive)
    (affe-grep org-directory)))

;; THEMEING
 (use-package ujelly-theme
   :ensure nil
   :config
   (load-theme 'ujelly t)
   (set-frame-parameter (selected-frame) 'alpha '(90 90))
   (add-to-list 'default-frame-alist '(alpha 90 90))
   (set-cursor-color "#dc322f")
   (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

   ;; (set-face-attribute 'mode-line nil :foreground "#bdc3ce" :background "#000")
   (set-face-attribute 'default nil :background "#000" :foreground "#eee"))

;;; WRITING
(use-package writegood-mode
  :hook (flyspell-mode . writegood-mode))
(use-package flyspell-correct
  :bind ("C-c DEL" . flyspell-correct-previous)
  :hook ((org-mode mu4e-compose-mode mail-mode git-commit-mode) . turn-on-flyspell))

;;; ORG
(if gv/is-termux
    (setq org-directory "~/storage/shared/Documents/org")
  (setq org-directory "~/Documents/org"))
(use-package org
  :pin org
  :ensure org-plus-contrib
  :commands (org-capture org-agenda)
  :bind (("C-c y" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :config
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
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "CANCELLED(c)")))

;;;; Agenda Views
  (setq org-agenda-custom-commands
	'(("d" "Today's Tasks"
	   ((agenda "" ((org-agenda-span 1)
			(org-agenda-overriding-header "Today's Tasks")))))
	  ;; ("d" "Dashboard"
	  ;;  ((agenda "" ((org-deadline-warning-days 7)))
	  ;;   (todo "NEXT"
	  ;; 	  ((org-agenda-overriding-header "Next Tasks")))
	  ;;   (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

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
	'(("t" "Todo" entry (file (concat org-directory "/refile.org"))
	   "* TODO %?\nDEADLINE: %T\n  %a")
	  ("m" "Meeting" entry (file+headline (concat org-directory "/Work.org") "Meetings")
	   "* Meeting with  %?\nSCHEDULED: %T\n")
	  ("r" "Refund" entry (file+olp (concat org-directory "/Work.org")
					"Work" "Refunds")
	   "* TODO Refund %?\n%?  %a\n")
	  ("w" "Waitlist" entry (file+olp (concat org-directory "/Work.org")
					  "Work" "Waitlist")
	   "* %?\n%? %a\n")
	  ("v" "Video Idea" entry (file+olp (concat org-directory "/youtube.org")
					    "YouTube" "Video Ideas")
	   "* %?\n%? %a\n")
	  ("c" "Cool Thing" entry (file+datetree (concat org-directory "/archive.org"))
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
    (set-face-attribute (car face) nil :font "Liberation Mono" :weight 'medium :height (cdr face))))
;;;; Contacts
(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '((concat org-directory "/contacts.org"))))
;;;; Drag And Drop
(use-package org-download
  :if (not gv/is-termux)
  :hook (dired-mode . org-download-enable)
  :init
  (setq org-agenda-files (seq-filter (lambda (x) (not (string-match "completed.org" x)))
				     (directory-files-recursively org-directory "\\.org$")))
  (setq-default org-download-screenshot-method "gnome-screenshot -a -f %s")
  (setq-default org-download-image-dir "./pic")
  (exwm-input-set-key (kbd "s-i") 'org-download-screenshot)
  :after org)
;;;; Better Looking Bullets
(use-package org-superstar
  :if (display-graphic-p)
  :after org
  :hook (org-mode . org-superstar-mode))
;;;; Templates
(use-package org-tempo
  :ensure nil
  :config
  (add-to-list 'org-structure-template-alist '("sh"  . "src sh"))
  (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("vim"  . "src vim")))
;;;; Indentation
(require 'org-indent)

;;; Git
(use-package magit
  :config
  (evil-define-key 'normal 'global (kbd "Q") 'magit))
(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;;; Completion
(use-package company
  :hook ((mu4e-compose-mode org-mode) . company-mode) ;org files
  :config
  (global-company-mode nil)
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1))

;;; VTERM AND ESHELL
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))
(use-package esh-autosuggest);company for eshell
(use-package em-alias
  :ensure nil
  :config
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (eshell/alias "e" "find-file $1")
	      (eshell/alias "ee" "find-file-other-window $1"))))
(use-package xterm-color)

;;; LANGS
(use-package fennel-mode)
(use-package racket-mode)

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
  :config
  (defun my-git-project-finder (dir)
    "Integrate .git project roots."
    (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
		       (expand-file-name dir))))
      (and dotgit
	   (cons 'transient (file-name-directory dotgit)))))
  (add-hook 'project-find-functions 'my-git-project-finder)) ; [built-in] Project Managment

;;; BUFFER MANAGMENT
(use-package ibuffer
  :ensure nil
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
		(name 18 18 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" "
		filename-and-process)))

  (define-key global-map (kbd "C-x C-b") #'ibuffer)
  ) ; [built-in] Powerful interface for managing buffers

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
	uniquify-separator " * "
	uniquify-after-kill-buffer-p t
	uniquify-strip-common-suffix t
	uniquify-ignore-buffers-re "^\\*")) ; [built-in] make buffers unique with filename rather than <num>
;;; DEFAULTS
(use-package emacs
  :ensure nil
  :config
  (set-frame-font "Liberation Mono 14" nil t)
  ;;;; Backups
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
	vc-make-backup-files t
	version-control t
	kept-old-versions 0
	kept-new-versions 10
	delete-old-versions t
	backup-by-copying t)
  ;;;; Defaults
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
    (scroll-bar-mode -1))             ; To disable the scroll bar, use the following line:
  (tool-bar-mode -1)               ; To disable the toolbar, use the following line:
  (fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"
  (show-paren-mode 1)              ; Highlight parenthesis
  (setq x-select-enable-primary t) ; use primary as clipboard in emacs
  (global-auto-revert-mode t)
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
	  try-expand-line))
  (define-key (current-global-map) [remap dabbrev-expand] 'hippie-expand)
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  ;; Vim like scrolling
  (setq scroll-step            1
	scroll-conservatively  10000)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;;;; Show All These In The Same Window
  (add-to-list 'same-window-buffer-names "*SQL*")
  (add-to-list 'same-window-buffer-names "*Help*")
  (add-to-list 'same-window-buffer-names "*Apropos*")
  (add-to-list 'same-window-buffer-names "*Process List*")) ; General emacs settings

(use-package savehist
  :init
  (savehist-mode t)) ; Save command history

;;; FOLDING
;;;; Cycle Headings With Bicycle
(use-package outline
  :ensure nil
  :config
  (let ((map outline-minor-mode-map))
    (define-key map (kbd "C-<tab>") #'outline-cycle)
    (define-key map (kbd "<backtab>") #'outline-cycle-buffer) ; S-TAB
    (define-key map (kbd "C-c C-n") #'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") #'outline-previous-visible-heading)
    (define-key map (kbd "C-c C-f") #'outline-forward-same-level)
    (define-key map (kbd "C-c C-b") #'outline-backward-same-level)
    (define-key map (kbd "C-c C-a") #'outline-show-all)
    (define-key map (kbd "C-c C-o") #'outline-hide-other)
    (define-key map (kbd "C-c C-u") #'outline-up-heading)))
(use-package bicycle
  :after outline
  :config
  (evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "<tab>") 'bicycle-cycle)
  (evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "<backtab>") 'bicycle-cycle-global))

;;;; Setup Folding For Programming
(use-package prog-mode
  :ensure nil
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;;; EXTRA UI
;;;; Beacon
(use-package beacon 
  :init
  (beacon-mode 1)); Highlight cursor postion after movement
;;;; Display hex colors in emacs
(use-package rainbow-mode
  :init
  (rainbow-mode t))

;;; DIRED
(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-aghoA --group-directories-first"))
  :config
;;;;; Hide . and .. in dired
  (setq dired-omit-files
	(rx (or (seq bol (? ".") "#")
		(seq bol "." eol)
		(seq bol ".." eol))))

  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
;;;;; vim vinigar style
  (evil-collection-define-key 'normal 'dired-mode-map
    "-" 'dired-up-directory)
  (define-key  evil-normal-state-map (kbd "-") (lambda () (interactive)
						 (dired ".")))
;;;;; xdg-open integration
  (require 'dired-x)
  (defun gv/dired-xdg-open ()
    "Open the file at point with xdg-open."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file)
      (message "Opening %s done" file)))
  (evil-collection-define-key 'normal 'dired-mode-map "O" 'gv/dired-xdg-open)
  (define-key dired-mode-map (kbd "O") 'gv/dired-xdg-open)
  )

;;; EMAIL
(unless gv/is-termux
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")) ; add mu4e to be loaded
(use-package mu4e
  :if (not gv/is-termux)
  :ensure nil
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Show full email address
  (setq mu4e-view-show-addresses 't)

  ;; where to put attachemnts
  (setq mu4e-attachment-dir  "~/Downloads")

  ;; use mu4e for e-mail in emacs
  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-maildir "~/.local/share/mail")

  ;; This prevents saving the email to the Sent folder since gmail will do this for us on their end.
  (setq mu4e-sent-messages-behavior 'delete)
  (setq message-kill-buffer-on-exit t)

;;;; Attachment reminders
  (use-package message-attachment-reminder)

  ;; (use-package message-are-flowing
  ;;   :config
  ;;   (with-eval-after-load "mu4e"
  ;;   (add-hook 'mu4e-compose-mode-hook 'messages-are-flowing-use-and-mark-hard-newlines))
  ;;   (with-eval-after-load "message"
  ;;     (add-hook 'message-mode-hook 'messages-are-flowing-use-and-mark-hard-newlines)))

;;;; mbsync and msmtp setup
  (setq mu4e-get-mail-command "mailsync"
	sendmail-program "/usr/bin/msmtp"
	message-sendmail-extra-arguments '("--read-envelope-from")
	send-mail-function 'smtpmail-send-it
	message-sendmail-f-is-evil t
	message-send-mail-function 'message-send-mail-with-sendmail)

  ;;images in emails
  (setq mu4e-view-show-images t)
;;;; abbreviations 
  (define-abbrev-table 'mu4e-compose-mode-abbrev-table
    '(("sin" "Sincerely, \nGavin Jaeger-Freeborn" nil 1)))
  (add-hook 'mu4e-compose-mode-hook #'abbrev-mode)
  (add-to-list 'auto-mode-alist '("^/tmp/neomutt.*\\'" . mail-mode))

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
				:query "(flag:flagged OR flag:unread OR NOT flag:replied) AND date:3m..now" :key 116)
			 (:name "Today's messages"
				:query "date:today..now" :key 118)
			 (:name "Last 7 days"
				:query "date:7d..now" :hide-unread t :key 119)
			 (:name "Messages with images"
				:query "mime:image/*" :key 112)))) ; email client depends on mu command

;;; PASS
(use-package password-store)
;;; EXWM
(use-package exwm
  :if (not gv/is-termux)
  :config
;;;; Hooks
  (add-hook 'exwm-update-class-hook
	    (lambda ()
	      (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-manage-finish-hook
	    (lambda ()
	      (when (and exwm-class-name
			 (or (string= exwm-class-name "qutebrowser")
			     (string= exwm-class-name "libreoffice-writer")
			     (string= exwm-class-name "libreoffice-calc")
			     (string= exwm-class-name "Google-chrome")
			     (string= exwm-class-name "Brave-browser")))
		(exwm-input-set-local-simulation-keys nil))))

  (add-hook 'exwm-update-title-hook
	    (lambda ()
	      (pcase exwm-class-name
		("qutebrowser" (exwm-workspace-rename-buffer (format "qb: %s" exwm-title)))
		("libreoffice-writer" (exwm-workspace-rename-buffer (format "Writer: %s" exwm-title)))
		("libreoffice-calc" (exwm-workspace-rename-buffer (format "Calc: %s" exwm-title)))
		("St" (exwm-workspace-rename-buffer (format "%s" exwm-title))))))
;;;; Global Key Bindings
  (setq exwm-input-global-keys
	`(([?\s-h] . windmove-left)
	  ([?\s-l] . windmove-right)
	  ;; Window Managment
	  ([?\s-j] . edwina-select-next-window)
	  ([?\s-k] . edwina-select-previous-window)
	  ([?\s-s] . edwina-dec-nmaster)
	  ([?\s-a] . edwina-inc-nmaster)
	  ([?\s-v] . edwina-zoom)
	  (,(kbd "s-]")           . edwina-inc-mfact)
	  (,(kbd "s-[")           . edwina-dec-mfact)
	  (,(kbd "s-q")           . edwina-delete-window)
	  (,(kbd "<s-backspace>") . edwina-delete-window)
	  (,(kbd "<s-return>")    . (lambda ()
				      (interactive)
				      (edwina-clone-window)
				      (balance-windows)
				      (other-window 1)))
	   ([?\s-f] . exwm-layout-set-fullscreen)
	   ([?\s-q] . edwina-delete-window) ; closing windows
	   ([?\s-c] . inferior-octave)
	   ([?\s-C] . (lambda ()
			(interactive)
			(kill-this-buffer)
			(edwina-delete-window)))
	   ;; reset exwm
	   ([?\s-r] . (lambda ()
			(interactive)
			(exwm-reset)
			(edwina-arrange)))
	   ;; tile exwm
	   ([?\s-t] . (lambda ()
			(interactive)
			(exwm-reset)
			(edwina-arrange)))

	   ;; open a terminal
	   (,(kbd "s-T") . (lambda ()
			     (interactive)
			     (edwina-clone-window)
				    (vterm)))
	   ;; launch any program
	   ([?\s-d] . (lambda (command)
			(interactive (list (read-shell-command "λ ")))
			(start-process-shell-command command nil command)))
	   ;; screen and audio controls
	   (,(kbd "C-s-f") . (lambda ()
			       (interactive)
			       (start-process-shell-command "Vol ↑" nil "cm up 5")))
	   (,(kbd "C-s-a") . (lambda ()
			       (interactive)
			       (start-process-shell-command "Vol ↓" nil "cm down 5")))
	   (,(kbd "C-s-d") . (lambda ()
			       (interactive)
			       (start-process-shell-command "Brightness ↑" nil "cl up 5")))
	   (,(kbd "C-s-s") . (lambda ()
			       (interactive)
			       (start-process-shell-command "Brightness ↓" nil "cl down 5")))
	   ;; web browser
	   ([?\s-w] . (lambda ()
			(interactive)
			(start-process-shell-command "ducksearch" nil "ducksearch")))

	   (,(kbd "s-E") . mu4e)
	   (,(kbd "s-e") . eshell)
	   ;;powermanager
	   ([?\s-x] . (lambda ()
			(interactive)
			(start-process-shell-command "power_menu.sh" nil "power_menu.sh")))
	   ([?\s-m] . (defun remind-timer (reminder)
			(interactive "reminder?")
			(egg-timer-do-schedule 3 reminder)))
	   ([?\s-b] . consult-buffer)
	   (,(kbd "C-x C-b") . ibuffer)
	   (,(kbd "s-B") . ibuffer)
	   ([?\s-=] . (lambda ()
			(interactive )
			(start-process-shell-command "Connections" nil
						     "dmenu_connection_manager.sh")))
	   ([?\s-p] . (lambda ()
			(interactive)
			(start-process-shell-command "Clipmenu" nil "clipmenu")))
;;;; Workspaces
	   ([?\s-g] . exwm-workspace-switch)
	   ,@(mapcar (lambda (i)
		       `(,(kbd (format "s-%d" i))
			 (lambda ()
			   (interactive)
			   (exwm-workspace-switch-create ,i))))
		     (number-sequence 1 9))))
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (fringe-mode 1)
;;;; Start EXWM
  (exwm-enable)
;;;; Start Programs For EXWM
  (start-process-shell-command "blueman-applet" nil "blueman-applet")
  (start-process-shell-command "nm-applet" nil "nm-applet")
;;;; Window Divider
  (setq window-divider-default-right-width 3)
  (let ((color (face-background 'mode-line)))
    (dolist (face '(window-divider-first-pixel
		    window-divider-last-pixel
		    window-divider))
      (set-face-foreground face color)))

  (window-divider-mode 1)
;;;; Mouse Settings
  :init (setq mouse-autoselect-window t
	      focus-follows-mouse t))
(use-package exwm-systemtray
  :ensure nil
  :after exwm
  :config
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 23))
(use-package exwm-randr
  :ensure nil
  :after exwm
  :config
  (setq exwm-randr-workspace-output-plist '(3 "HDMI2"))
  (add-hook 'exwm-randr-screen-change-hook
	    (lambda ()
	      (start-process-shell-command
	       "xrandr" nil "xrandr --output eDP1 --primary --auto --left-of HDMI2 --auto")))
  (exwm-randr-enable))
(use-package exwm-mff
  :after exwm
  :config
  (exwm-mff-mode t)) ; mouse follows focus
(use-package edwina
  :after exwm
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))) ; dwm style window managment
(use-package winner
  :ensure nil
  :config
  (exwm-input-set-key (kbd "s-u") 'winner-undo)
  (exwm-input-set-key (kbd "s-r") 'winner-redo)
  :init
  (winner-mode 1)) ; window managment undo
(use-package vlf
  
  :ensure t
  :defer t) ; help emacs handle large files to avoid exwm from locking

;;; MODELINE
;; (use-package mini-modeline
;;   :if (not gv/is-termux)
;;   :init
;;   (require 'battery)
;;   (setq mini-modeline-r-format
;; 	(list
;; 	 ;; value of `mode-name'
;; 	 ;; value of current buffer name
;; 	 '(:eval (propertize "%b, " 'face 'font-lock-variable-name-face))
;; 	 '(:eval (propertize (if (eq 'emacs evil-state) "  " "  ")
;; 			     'face 'epa-validity-high))

;; 	 ;; value of current line number
;; 	 '(:eval (propertize " %l,%c" 'face 'font-lock-comment-face))
;; 	 '(:eval (propertize " %p" 'face 'font-lock-comment-face))
;; 	 " "
;; 	 ;; major mode
;; 	 '(:eval (propertize " (%m) " 'face 'font-lock-comment-face))
;; 	 ;; ;; spaces to align right
;; 	 ;; '(:eval (propertize
;; 	 ;; 		" " 'display
;; 	 ;; 		`((space :align-to (- (+ right right-fringe right-margin)
;; 	 ;; 				      ,(+ 10 (string-width mode-name)))))))
;; 	 '(:eval (propertize
;; 		  (format-time-string "%a, %b %d %I:%M%p")
;; 		  'face 'change-log-list))
;; 	 " "
;; 	 '(:eval (propertize
;; 		  (battery-format "[%p]" (funcall battery-status-function))
;; 		  'face 'org-checkbox))
;; 	 "      "))
;;   :config
;;   (mini-modeline-mode t))

(require 'battery)
;; (setq-default mode-line-format
;; 	(list
;; 	 ;; value of `mode-name'
;; 	 ;; value of current buffer name
;; 	 " "
;; 	 '(:eval (propertize "%b, " 'face 'font-lock-variable-name-face))
;; 	 '(:eval (propertize (if (eq 'emacs evil-state) "  " "  ")
;; 			     'face 'epa-validity-high))

;; 	 ;; value of current line number
;; 	 '(:eval (propertize " %l,%c" 'face 'font-lock-comment-face))
;; 	 '(:eval (propertize " %p" 'face 'font-lock-comment-face))
;; 	 " "
;; 	 ;; major mode
;; 	 '(:eval (propertize " (%m) " 'face 'font-lock-comment-face))
;; 	 ;; spaces to align right
;; 	 '(:eval (propertize
;; 			" " 'display
;; 			`((space :align-to (- (+ right right-fringe right-margin)
;; 					      ,(+ 17 (string-width mode-name)))))))
;; 	 '(:eval (propertize
;; 		  (format-time-string "%a, %b %d %I:%M%p")
;; 		  'face 'change-log-list))
;; 	 " "
;; 	 '(:eval (propertize
;; 		  (battery-format "[%p]" (funcall battery-status-function))
;; 		  'face 'org-checkbox))
;; 	 ))
(setq-default mode-line-format
	      (list
	       ;; value of `mode-name'
	       ;; value of current buffer name
	       " "
	       "%b, "
	       '(:eval (if (eq 'emacs evil-state) "  " "  "))
	       'mode-line-buffer-identification
	       ;; value of current line number
	       " %l,%c"
	       " %p"
	       " "
	       ;; major mode
	       " (%m) "
	       ;; spaces to align right
	       '(:eval (propertize
			" " 'display
			`((space :align-to (- (+ right right-fringe right-margin)
					      ,(+ 17 (string-width mode-name)))))))
	       '(:eval (format-time-string "%a, %b %d %I:%M%p"))
	       " "
	       '(:eval (battery-format "[%p]" (funcall battery-status-function)))))
;;; Server Setup
(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))
;;; Extras
;; (use-package quelpa-use-package)
;; (use-package matrix-client
;;   :if (not gv/is-termux)
;;   :quelpa (matrix-client :fetcher github :repo "alphapapa/matrix-client.el"
;;                          :files (:defaults "logo.png" "matrix-client-standalone.el.sh")))
;; (use-package eaf
;;   :if (not gv/is-termux)
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
;;   :init
;;   (use-package epc :defer t :ensure t)
;;   (use-package ctable :defer t :ensure t)
;;   (use-package deferred :defer t :ensure t)
;;   (use-package s :defer t :ensure t)
;;   (use-package eaf-evil :ensure nil)
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   :config
;;   (eaf-setq eaf-browser-default-search-engine "duckduckgo")
;;   (eaf-setq eaf-browser-enable-adblocker "true")
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding))

;;;; Better PDFs
;https://github.com/politza/pdf-tools
; annotate pdfs with c-c c-a
; hl with c-c c-a h
; for help M-x pdf-tools-help RET
(use-package pdf-tools
  :config
  ;; pdf auto refresh
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))
(use-package transmission)
;;;; proced [built-in] htop alternative
(use-package proced
  :ensure nil
  :config
  (setq proced-auto-update-flag t
	proced-auto-update-interval 2
	proced-decend t))
;;;; stuff to ignore
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3a9f65e0004068ecf4cf31f4e68ba49af56993c20258f3a49e06638c825fbfb6" "2b502f6e3bf0cba42fe7bf83a000f2d358a7020a7780a8682fcfed0c9dbffb5f" default))
 '(org-agenda-files
   '("/home/gavinok/Documents/org/2021_canoekids_waitlist.org" "/home/gavinok/Documents/org/2021_flatwaternorth_executive_meeting.org" "/home/gavinok/Documents/org/2021_june_flatwaternorth_executive_meeting.org" "/home/gavinok/Documents/org/2021_may_flatwaternorth_executive_meeting.org" "/home/gavinok/Documents/org/Athletes.org" "/home/gavinok/Documents/org/Practices.org" "/home/gavinok/Documents/org/Work.org" "/home/gavinok/Documents/org/archive.org" "/home/gavinok/Documents/org/backlog.org" "/home/gavinok/Documents/org/c.org" "/home/gavinok/Documents/org/contacts.org" "/home/gavinok/Documents/org/divisionals_meeting.org" "/home/gavinok/Documents/org/elm.org" "/home/gavinok/Documents/org/fennel.org" "/home/gavinok/Documents/org/guile.org" "/home/gavinok/Documents/org/hy.org" "/home/gavinok/Documents/org/interviews_flatwaternorth.org" "/home/gavinok/Documents/org/janet.org" "/home/gavinok/Documents/org/joker.org" "/home/gavinok/Documents/org/june_ptso.org" "/home/gavinok/Documents/org/mylife.org" "/home/gavinok/Documents/org/nationals_prep.org" "/home/gavinok/Documents/org/parents_meeting.org" "/home/gavinok/Documents/org/racket.org" "/home/gavinok/Documents/org/rec.org" "/home/gavinok/Documents/org/refile.org" "/home/gavinok/Documents/org/reminders.org" "/home/gavinok/Documents/org/results.org" "/home/gavinok/Documents/org/roadshow.org" "/home/gavinok/Documents/org/staff_orientation.org" "/home/gavinok/Documents/org/staff_orientation_day.org" "/home/gavinok/Documents/org/today.org" "/home/gavinok/Documents/org/youtube.org" "/home/gavinok/Documents/org/yukon_river_quest.org"))
 '(package-selected-packages
   '(bicycle password-store vlf system-packages popwin exwm-edit forecast xterm-color writegood-mode vterm vs-dark-theme volume vimrc-mode vertico undo-fu-session undo-fu ujelly-theme tron-legacy-theme transmission symon rainbow-mode racket-mode quelpa-use-package quasi-monochrome-theme pdf-tools org-superstar org-plus-contrib org-download org-alert orderless modus-themes mini-modeline message-attachment-reminder matrix-client marginalia magit lua-mode keycast git-gutter flyspell-correct fennel-mode exwm-mff exwm evil-terminal-cursor-changer evil-surround evil-lion evil-commentary evil-collection esh-autosuggest epc eglot edwina eaf dired-open beacon all-the-icons affe ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
