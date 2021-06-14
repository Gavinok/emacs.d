;; Set up package.el to work with MELPA
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
;;(package-refresh-contents)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background
				       (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))
(add-hook 'evil-mode-hook 'hexcolour-add-to-font-lock)

;; Evil mode settings
(progn

  ;; Vim like scrolling
  (setq scroll-step            1
	scroll-conservatively  10000)
  ;; Vim style undo
  ;; (unless (package-installed-p 'undo-fu)
  ;;   (package-install 'undo-fu))
  (use-package undo-fu)

  (use-package  evil
		:init
		(setq evil-evil-want-Y-yank-to-eol t)
		(setq evil-want-keybinding nil)
		(setq evil-split-window-below t)
		(setq evil-split-window-right t)
		(setq evil-undo-system 'undo-fu)
		:config
		(evil-mode 1)
		(evil-set-leader 'normal " ")
		(evil-define-key 'normal 'global (kbd "<leader>b") 'ivy-switch-buffer)
		(evil-define-key 'normal 'global (kbd "<leader>f") 'counsel-find-file)
		(evil-define-key 'normal 'global (kbd "<leader>t") 'capitalize-dwim)
		(evil-define-key 'visual 'global (kbd "<leader>t") 'capitalize-dwim)
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

;; Terminal Settings
(if (display-graphic-p)
    (set-face-background 'default "#000000")
  (progn (set-face-background 'default "undefined")
	 (add-to-list 'term-file-aliases
		      '("st-256color" . "xterm-256color"))))

;; Setup Ivy
(use-package counsel
	     :config
	     (counsel-mode 1)
	     (global-set-key (kbd "C-s") 'swiper-isearch)
	     (global-set-key (kbd "M-x") 'counsel-M-x)
	     (global-set-key (kbd "C-x C-f") 'counsel-find-file)
	     (global-set-key (kbd "M-y") 'counsel-yank-pop)
	     (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
	     (global-set-key (kbd "C-c v") 'ivy-push-view)
	     (global-set-key (kbd "C-c V") 'ivy-pop-view))

;; Themeing
;; todo
(use-package badger-theme
	     :config
	     (load-theme 'badger t)
	     (set-cursor-color "#dc322f")
	     (set-face-attribute 'region nil :background "#666" :foreground "#ffffff"))

(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; Org Stuff
(use-package org-download
	     :init
	     (setq org-directory "~/Documents/org")
	     (setq org-agenda-files '("today.org" "refile.org"
				      "work.org" "Practices.org"))
	     :config
	     (add-hook 'dired-mode-hook 'org-download-enable)
	     (setq-default org-download-image-dir "./pic"))


(use-package org-bullets
	     :config
	     (if (display-graphic-p)
		 (add-hook 'org-mode-hook #'org-bullets-mode)))

;; Make sure org-indent face is available
(require 'org-indent)
(require 'org-tempo)

(evil-define-key 'normal 'global (kbd "gA") 'org-agenda)

(setq org-default-notes-file (concat org-directory "/refile.org"))
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

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
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

;; Magit
(use-package magit
  :config
  (evil-define-key 'normal 'global (kbd "Q") 'magit))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;; pdf auto refresh
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; No startup
(setq inhibit-startup-screen t)
;; disable backup
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)
;; add borders
(set-frame-parameter nil 'internal-border-width 10)
;; To disable the menu bar, place the following line in your .emacs file:
(menu-bar-mode -1)
;; To disable the scroll bar, use the following line:
(toggle-scroll-bar -1)
;; To disable the toolbar, use the following line:
(tool-bar-mode -1)
;; don't ask to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)


;; Highlight parenthesis
(show-paren-mode 1)

;; Completion BEGIN --------------------------------------------------
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
;; Completion END --------------------------------------------------

;;archive completed tasks
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

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

(use-package  racket-mode)
;; Langs END ----------------------------------------------

;; Dired settings --------------------------------------------------

;; A poor mans vim vinegar 
(define-key  evil-normal-state-map (kbd "-") (lambda () (interactive)
						(dired ".")))
(defun gavin-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1)
  (define-key dired-mode-map (kbd "-") (lambda () (interactive) (find-alternate-file ".."))))
(add-hook 'dired-mode-hook 'gavin-dired-mode-setup)


(set-frame-font "Liberation Mono 14" nil t)

(add-to-list 'auto-mode-alist '("^/tmp/neomutt.*\\'" . mail-mode))
(add-hook 'mail-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'turn-on-flyspell) ;spell checking

;;non essential packages
(use-package transmission)

;; EXWM
(use-package exwm
  :init (setq mouse-autoselect-window t
	      focus-follows-mouse t))
(use-package exwm-config)
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(exwm-config-example)
(setq exwm-input-global-keys
      `( ([?\s-h] . windmove-left)
	 ([?\s-l] . windmove-right)
	 ;; ([?\s-j] . windmove-down)
	 ;; ([?\s-k] . windmove-up)
	 ([?\s-j] . edwina-select-next-window)
	 ([?\s-k] . edwina-select-previous-window)
	 ([?\s-s] . edwina-dec-nmaster)
	 ([?\s-a] . edwina-inc-nmaster)
	 ([?\s-v] . edwina-zoom)
	 ([?\s-r] . exwm-reset)
	 ([?\s-w] . exwm-workspace-switch)
	 ([?\s-b] . counsel-switch-buffer)
	 ([?\s-z] . exwm-floating-toggle-floating)
	 ([?\s-f] . exwm-layout-set-fullscreen)
	 ([?\s-q] . kill-this-buffer)
	 ([?\s-p] . counsel-yank-pop)
	 ([?\s-d] . (lambda (command)
		      (interactive (list (read-shell-command "λ ")))
		      (start-process-shell-command command nil command)))

	,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i))
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(exwm-input-set-key (kbd "s-]") 'edwina-inc-mfact)
(exwm-input-set-key (kbd "s-[") 'edwina-dec-mfact)
(exwm-input-set-key (kbd "s-<return>") 'edwina-clone-window)

(use-package edwina
  :ensure t
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-mode 1))

;; (setq exwm-input-simulation-keys
;;       '(([?\C-b] . [left])
;;         ([?\C-f] . [right])
;;         ([?\C-p] . [up])
;;         ([?\C-n] . [down])
;;         ([?\C-a] . [home])
;;         ([?\C-e] . [end])
;;         ([?\M-v] . [prior])
;;         ([?\C-v] . [next])
;;         ([?\C-d] . [delete])
;;         ([?\C-k] . [S-end delete])))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (or (string= exwm-class-name "qutebrowser")
			   (string= exwm-class-name "Brave-browser")))
              (exwm-input-set-local-simulation-keys nil))))

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("43f03c7bf52ec64cdf9f2c5956852be18c69b41c38ab5525d0bedfbd73619b6a" default))
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files '("~/Documents/org/today.org"))
 '(package-selected-packages
   '(magit git-gutter org-download exwm-mff evil-mff evil-exwm-state typo-suggest type-suggest company helpful racket-mode fennel-mode undo-fu undo-fu-session org-bullets evil-collection ivy evil-lion evil-surround evil-commentary evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
