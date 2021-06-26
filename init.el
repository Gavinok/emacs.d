;; Set up package.el to work with MELPA (require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

;; BOOTSTRAP USE-PACKAGE ------------------
(package-initialize)
;;(package-refresh-contents)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
    (package-install 'use-package))
(eval-when-compile (require 'use-package))
;; END BOOTSTRAP USE-PACKAGE ------------------

(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background
				       (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))
(add-hook 'evil-mode-hook 'hexcolour-add-to-font-lock)
(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; EVIL MODE ----------------------------------
(progn
  ;; Vim like scrolling
  (setq scroll-step            1
	scroll-conservatively  10000)
  ;; Vim style undo
  ;; (unless (package-installed-p 'undo-fu)
  ;;   (package-install 'undo-fu))
  (use-package undo-fu)
  (use-package undo-fu-session
    :after undo-fu
    :init
    (global-undo-fu-session-mode))
  (use-package  evil
    :after undo-fu
    :init
    (setq evil-evil-want-Y-yank-to-eol t)
    (setq evil-want-keybinding nil)
    (setq evil-split-window-below t)
    (setq evil-split-window-right t)
    (setq evil-undo-system 'undo-fu)
    :config
    (evil-mode 1)
    (evil-set-leader 'normal " ")
    (evil-define-key 'normal 'global (kbd "<leader>b") 'consult-buffer)
    (evil-define-key 'normal 'global (kbd "<leader>t") 'capitalize-dwim)
    (evil-define-key 'visual 'global (kbd "<leader>t") 'capitalize-dwim)
    (evil-define-key 'normal 'global (kbd "<backspace>") 'flyspell-check-previous-highlighted-word)
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

  ;; evil mode in other modes live viewing pdfs
  (use-package  evil-collection
    :config
    (evil-collection-init))
  ;; enable evil

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
    (define-key evil-normal-state-map (kbd "gl") (lambda () (interactive)
						   (evil-lion-left)))
    (define-key evil-normal-state-map (kbd "gL") (lambda () (interactive)
						   (evil-lion-right))))
  ;; Cursor Shape
  (use-package evil-terminal-cursor-changer
    :config
    (unless (display-graphic-p)
      (evil-terminal-cursor-changer-activate))))
;; END EVIL MODE ----------------------------------

;; TERMINAL SETTINGS -------------------
(if (display-graphic-p)
    (set-face-background 'default "#000000")
  (progn (set-face-background 'default "undefinded")
	 (add-to-list 'term-file-aliases
		      '("st-256color" . "xterm-256color"))
	 (xterm-mouse-mode t))
  (global-set-key (kbd "<mouse-4>") 'next-line)
  (global-set-key (kbd "<mouse-5>") 'previous-line))
;; END TERMINAL SETTINGS -------------------

;; COMPLETION -------------------------------------

;; Setup Ivy
(use-package counsel
  :config
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view))

(use-package vertico
  :init
  (vertico-mode))
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))
(use-package consult
  :after vertico
  :bind (("C-s" . consult-line)
	 ("C-M-l" . consult-imenu)
	 ("C-M-j" . persp-switch-to-buffer*)
	 :map minibuffer-local-map)
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>j") 'consult-imenu))
(use-package consult-recoll
  :config
  (evil-define-key 'normal 'global (kbd "gO") 'consult-recoll))
(use-package affe
  :after orderless
  :config
  ;; only exclude git files
  (setq affe-find-command "find  -not -path '*/\\.nnn*' -not -path '*/\\.git*' -type f")
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
	affe-highlight-function #'orderless--highlight)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  (evil-define-key 'normal 'global (kbd "<leader>g") 'affe-grep)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'affe-find))

;; END COMPLETION -------------------------------------

;; Themeing
(use-package badger-theme
  :config
  (load-theme 'badger t)
  (set-cursor-color "#dc322f")
  (set-face-attribute 'region nil :background "#666" :foreground "#ffffff"))

(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; ORG -------------------------
(use-package org
  :ensure org-plus-contrib
  :config
  (evil-define-key 'normal 'global (kbd "<leader>y") 'org-store-link)
  (evil-define-key 'normal 'global (kbd "gA") 'org-agenda)
  (evil-define-key 'normal 'global (kbd "gC") 'org-capture)

  (setq org-default-notes-file (concat org-directory "/refile.org"))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELLED(k@)")))

  ;; Configure custom agenda views
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
	    (org-agenda-files org-agenda-files)))
	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANCELLED"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/Documents/org/refile.org")
	   "* TODO %?\nDEADLINE: %T\n  %a")
	  ("m" "Meeting" entry (file+headline "~/Documents/org/mylife.org" "Meetings")
	   "* Meeting with  %?\nSCHEDULED: %T\n")
	  ("r" "Refund" entry (file+olp "~/Documents/org/Work.org"
					"Work" "Refunds")
	   "* TODO Refund %?\n%?  %a\n")
	  ("w" "Waitlist" entry (file+olp "~/Documents/org/Work.org"
					  "Work" "Waitlist")
	   "* %?\n%? %a\n")
	  ("v" "Video Idea" entry (file+olp "~/Documents/org/youtube.org"
					    "YouTube" "Video Ideas")
	   "* %?\n%? %a\n")
	  ("c" "Cool Thing" entry (file+datetree "~/Documents/org/archive.org")
	   "* %?\nEntered on %U\n  %i\n  %a"))))

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Documents/org/contacts.org")))

(use-package org-download
  :init
  (setq org-directory "~/Documents/org")
  (setq org-agenda-files (seq-filter (lambda(x) (not (string-match "completed.org" x)))
       (directory-files-recursively org-directory "\\.org$")))
  (setq-default org-download-screenshot-method "gnome-screenshot -a -f %s")
  (setq-default org-download-image-dir "./pic")
  (exwm-input-set-key (kbd "s-i") 'org-download-screenshot)
  :after org
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-bullets
  :after org
  :config
  (if (display-graphic-p)
      (add-hook 'org-mode-hook #'org-bullets-mode)))

;; Make sure org-indent face is available
(require 'org-indent)
(require 'org-tempo)

;; (use-package org-alert
;;   :config
;;   (setq alert-default-style 'libnotify)
;;   (org-alert-enable))
;; (use-package org-notifications
;;   :config
;;   (org-notifications-start))

;;archive completed tasks
(defun my-org-archive-done-tasks ()
  (interactive)
  (progn
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (org-map-entries 'org-archive-subtree "/CANCELLED" 'file)))

(add-hook 'org-mode-hook 'turn-on-flyspell) ;spell checking

;; END ORG -------------------------

;; GIT -------------------------
;; Magit
(use-package magit
  :config
  (evil-define-key 'normal 'global (kbd "Q") 'magit))
(use-package git-gutter
  :config
  (global-git-gutter-mode +1))
;; END GIT -------------------------

;; No startup
;; disable backup
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq inhibit-startup-screen t)
;; Add border
(menu-bar-mode -1)               ; To disable the menu bar, place the following line in your .emacs file:
(scroll-bar-mode -1)             ; To disable the scroll bar, use the following line:
(tool-bar-mode -1)               ; To disable the toolbar, use the following line:
(fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"
(show-paren-mode 1)              ; Highlight parenthesis
(setq x-select-enable-primary t) ; use primary as clipboard in emacs

;; CODE COMPLETION --------------------------------------------------
(setq hippie-expand-try-functions-list
      '(
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	;; try-expand-dabbrev-from-kill
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	try-complete-file-name-partially
	try-complete-file-name
	;; try-expand-all-abbrevs
	;; try-expand-list
	;; try-expand-line
	))
(use-package company
  :config (global-company-mode t)
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1))
(use-package corfu
  :bind (:map corfu-map
	      ("TAB" . corfu-next)
	      ("S-TAB" . corfu-previous))
  :config (setq tab-always-indent 'complete)
  :init
  (corfu-global-mode))
;; Dabbrev works with Corfu
(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand)))
;; END CODE COMPLETION --------------------------------------------------

;; Langs ----------------------------------------------

; better lisp bindings
(define-key  evil-normal-state-map (kbd "(") (lambda () (interactive)
					       (evil-previous-open-paren)))
(define-key  evil-normal-state-map (kbd ")") (lambda () (interactive)
					       (evil-next-close-paren)))
(define-key  evil-normal-state-map (kbd "(") (lambda () (interactive)
					       (evil-previous-open-paren)))
(define-key  evil-operator-state-map (kbd "(") (lambda () (interactive)
					       (evil-previous-open-paren)))
(define-key  evil-operator-state-map (kbd ")") (lambda () (interactive)
					       (evil-next-close-paren)))
(use-package fennel-mode)
(use-package racket-mode)
;;; LSP using eglot
(use-package eglot)

;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(defun my-git-project-finder (dir)
  "Integrate .git project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
		     (expand-file-name dir))))
    (and dotgit
	 (cons 'transient (file-name-directory dotgit)))))


(use-package project
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :config
(add-hook 'project-find-functions 'my-git-project-finder))

;; Langs END ----------------------------------------------

;; dired settings --------------------------------------------------
;; A poor mans vim vinegar 
;; NATIVE SETTINGS -----------------------------------------
(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "-" 'dired-up-directory)
  (define-key  evil-normal-state-map (kbd "-") (lambda () (interactive)
						 (dired ".")))
  ;; (defun gavin-dired-mode-setup ()
  ;;   "to be run as hook for `dired-mode'."
  ;;   (dired-hide-details-mode 1)
  ;;   (define-key dired-mode-map (kbd "-")
  ;;     (lambda () (interactive) (find-alternate-file ".."))))
  ;; (add-hook 'dired-mode-hook 'gavin-dired-mode-setup)
  )

(use-package dired-open
  :config
  (setq dired-open-extensions '(("pdf" . "zathura")
				("ps"  . "zathura")
				("mkv" . "mpv")
				("mp4" . "mpv")
				("mp3" . "mpv"))))

(set-frame-font "Liberation Mono 14" nil t)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
;; END NATIVE SETTINGS -----------------------------------------
;; EMAIL --------------------------------------------------------
;; where to find mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(use-package mu4e
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

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "mailsync"
	sendmail-program "/usr/bin/msmtp"
	message-sendmail-extra-arguments '("--read-envelope-from")
	send-mail-function 'smtpmail-send-it
	message-sendmail-f-is-evil t
	message-send-mail-function 'message-send-mail-with-sendmail)
  
  ;;images in emails
  (setq mu4e-view-show-images t)
  (define-abbrev-table 'mu4e-compose-mode-abbrev-table
    '(("sin" "Sincerely, \nGavin" nil 1)))
  (add-hook 'mu4e-compose-mode-hook #'abbrev-mode)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (add-to-list 'auto-mode-alist '("^/tmp/neomutt.*\\'" . mail-mode))
  (add-hook 'mail-mode-hook 'flyspell-mode)

  ;; something about ourselves
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
  ;; Contacts
  (setq mail-personal-alias-file  "~/.config/mutt/aliases")
  (setq mu4e-org-contacts-file  "~/Documents/org/contacts.org")
  (add-to-list 'mu4e-headers-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t))
 ;;End EMAIL ----------------------------------------------------------- 

;;NON-ESSENTIAL PACKAGES
(use-package beacon
  :init
  (beacon-mode 1))
(use-package esh-autosuggest)

;; (setq eshell-aliases-file "~/.emacs.d/aliases")
;https://github.com/politza/pdf-tools
; annotate pdfs with c-c c-a
; hl with c-c c-a h
; for help M-x pdf-tools-help RET
(use-package pdf-tools
  :config
  ;; pdf auto refresh
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

(use-package transmission)
;; EXWM ----------------
(use-package exwm
  :config
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
		("qutebrowser" (exwm-workspace-rename-buffer (format "Qute: %s" exwm-title))))))
  (setq exwm-input-global-keys
	`( ([?\s-h] . windmove-left)
	   ([?\s-l] . windmove-right)
	   ([?\s-0] . (lambda ()
			(interactive )
			(start-process-shell-command "dmenu_connection_manager.sh" nil "dmenu_connection_manager.sh")))
	   ([?\s-j] . edwina-select-next-window)
	   ([?\s-k] . edwina-select-previous-window)
	   ([?\s-s] . edwina-dec-nmaster)
	   ([?\s-a] . edwina-inc-nmaster)
	   ([?\s-v] . edwina-zoom)
					; closing windows
	   ([?\s-q] . edwina-delete-window)
	   ([?\s-C] . (lambda ()
			(interactive)
			(progn (kill-this-buffer)
			       (edwina-delete-window))))
	   ;; reset exwm
	   ([?\s-r] . (lambda ()
			(interactive)
			(progn (exwm-reset)
			       (edwina-arrange))))
	   ;; tile exwm
	   ([?\s-t] . (lambda ()
			(interactive)
			(progn (exwm-reset)
			       (edwina-arrange))))
	   ;; launch any program
	   ([?\s-d] . (lambda (command)
			(interactive (list (read-shell-command "λ ")))
			(start-process-shell-command command nil command)))
	   ;; web browser
	   ([?\s-w] . (lambda ()
			(interactive)
			(start-process-shell-command "ducksearch" nil "ducksearch")))

	   ([?\s-e] . (lambda ()
			(interactive)
			(progn (edwina-clone-window)
			       (mu4e))))
	   ;;powermanager
	   ([?\s-x] . (lambda ()
			(interactive)
			(start-process-shell-command "power_menu.sh" nil "power_menu.sh")))
	   ([?\s-m] . (defun remind-timer (reminder)
			(interactive "reminder?")
			(egg-timer-do-schedule 3 reminder)))
	   ([?\s-g] . exwm-workspace-switch)
	   ([?\s-f] . exwm-layout-set-fullscreen)
	   ([?\s-p] . consult-yank-pop)
	   ([?\s-b] . consult-buffer)
	   
	   ,@(mapcar (lambda (i)
		       `(,(kbd (format "s-%d" i))
			 (lambda ()
			   (interactive)
			   (exwm-workspace-switch-create ,i))))
		     (number-sequence 1 9))))

  ;; screen and audio controls
  (exwm-input-set-key (kbd "C-s-f") '(lambda ()
				       (interactive)
				       (start-process-shell-command "cm up 5" nil "cm up 5")))
  (exwm-input-set-key (kbd "C-s-a") '(lambda ()
				       (interactive)
				       (start-process-shell-command "cm down 5" nil "cm down 5")))
  (exwm-input-set-key (kbd "C-s-d") '(lambda ()
				       (interactive)
				       (start-process-shell-command "cl up 5" nil "cl up 5")))
  (exwm-input-set-key (kbd "C-s-s") '(lambda ()
				       (interactive)
				       (start-process-shell-command "cl dowm 5" nil "cl down 5")))
  (exwm-input-set-key (kbd "s-]") 'edwina-inc-mfact)
  (exwm-input-set-key (kbd "s-]") 'edwina-inc-mfact)
  (exwm-input-set-key (kbd "s-[") 'edwina-dec-mfact)
  (exwm-input-set-key (kbd "s-q") 'edwina-delete-window)
  (exwm-input-set-key (kbd "s-<return>") 'edwina-clone-window)
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (fringe-mode 1)
  (exwm-enable)
  :init (setq mouse-autoselect-window t
	      focus-follows-mouse t))
(use-package exwm-systemtray
  :ensure nil
  :after exwm
  :config
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 25))
(use-package exwm-randr
  :ensure nil
  :config
  (setq exwm-randr-workspace-output-plist '(3 "HDMI2"))
  (add-hook 'exwm-randr-screen-change-hook
	    (lambda ()
	      (start-process-shell-command
	       "xrandr" nil "xrandr --output eDP1 --primary --auto --left-of HDMI2 --auto")))
  (exwm-randr-enable))
(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected)))
(start-process-shell-command "blueman-applet" nil "blueman-applet")
(start-process-shell-command "nm-applet" nil "nm-applet")
;; END EXWM ----------------

;; MODELINE --------------------------------------
(use-package mini-modeline
  :init
  (setq mini-modeline-r-format
	(list
	 ;; value of `mode-name'
	 ;; value of current buffer name
	 '(:eval (propertize "%b, " 'face 'font-lock-variable-name-face))
	 '(:eval (propertize (if (eq 'emacs evil-state) "  " "  ")
			     'face 'epa-validity-high))
	 ;; value of current line number
	 "l:%l "
	 ;; major mode
	 "%m: "
	 ;; ;; spaces to align right
	 ;; '(:eval (propertize
	 ;; 		" " 'display
	 ;; 		`((space :align-to (- (+ right right-fringe right-margin)
	 ;; 				      ,(+ 10 (string-width mode-name)))))))
	 '(:eval (propertize
		  (format-time-string "%a, %b %d %I:%M%p")
		  'face 'change-log-list))
	 " "
	 '(:eval (propertize
		  (battery-format "[%p%%]" (funcall battery-status-function))
		  'face 'change-log-list))
	 "    "))
  :config
  (mini-modeline-mode t))
;; END MODELINE --------------------------------------

;; start network manager applet

;;For org capture in qute
(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("43f03c7bf52ec64cdf9f2c5956852be18c69b41c38ab5525d0bedfbd73619b6a" default))
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files
   '("/home/gavinok/Documents/org/Work.org" "/home/gavinok/Documents/org/today.org"))
 '(package-selected-packages
   '(affe symon vertico consult org-notifications egg-timer org-plus-contrib volume ivy-youtube ytel esh-autosuggest pomidor ivy-clipmenu pdf-tools mini-modeline org-gcal org-alert edwina orderless corfu magit git-gutter org-download exwm-mff evil-mff evil-exwm-state typo-suggest type-suggest company helpful racket-mode fennel-mode undo-fu undo-fu-session org-bullets evil-collection ivy evil-lion evil-surround evil-commentary evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
