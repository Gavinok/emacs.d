;; Set up package.el to work with MELPA
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

;; spelling
;; M-X ispell
;;  will spell check the buffer
;; M-X ispell-word
;;  Checks the current word for a misspelling
;; M-X flyspell-mode
;;  spell check asynchronous 
(package-initialize)
;;(package-refresh-contents)

;; Evil Stuff
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background
				       (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))
(add-hook 'evil-mode-hook 'hexcolour-add-to-font-lock)

(progn
  (unless (package-installed-p 'undo-fu)
    (package-install 'undo-fu))
  (require 'undo-fu)
  ; (global-undo-fu-mode)

  (unless (package-installed-p 'evil)
    (package-install 'evil))
  ;; enable evil
  (setq evil-evil-want-Y-yank-to-eol t)
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  (setq evil-undo-system 'undo-fu)
  (require 'evil)
  (evil-mode 1)

  (unless (package-installed-p 'evil-collection)
    (package-install 'evil-collection))
  ;; enable evil
  (require 'evil-collection)
  (evil-collection-init)

  ;; Enable Commentary
  (unless (package-installed-p 'evil-commentary)
    (package-install 'evil-commentary))
  (require 'evil-commentary)
  (evil-commentary-mode 1)

  ;; Enable Surround
  (unless (package-installed-p 'evil-surround)
    (package-install 'evil-surround))
  (require 'evil-surround)
  (global-evil-surround-mode 1)

  ;; Enable Lion
  (unless (package-installed-p 'evil-lion)
    (package-install 'evil-lion))
  (require 'evil-lion)
  (evil-lion-mode 1)
  (define-key evil-normal-state-map (kbd "gl") (lambda () (interactive)
						 (evil-lion-left)))
  (define-key evil-normal-state-map (kbd "gL") (lambda () (interactive)
						 (evil-lion-right)))
  ;; cursor shape
  (unless (package-installed-p 'evil-terminal-cursor-changer)
    (package-install 'evil-terminal-cursor-changer))
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)))


;; terminal settings
(if (display-graphic-p)
    (set-face-background 'default "#000000")
  (progn (set-face-background 'default "undefined")
	 (add-to-list 'term-file-aliases
		      '("st-256color" . "xterm-256color"))))

;; (defun noct:conditionally-toggle-xclip-mode ()
;;   (if (display-graphic-p)
;;       (if (bound-and-true-p xclip-mode)
;; 	  (xclip-mode -1))
;;     (xclip-mode)))

;; (noct:conditionally-toggle-xclip-mode)

;; (add-hook 'focus-in-hook
;;           #'noct:conditionally-toggle-xclip-mode)

(evil-set-leader 'normal " ")
(evil-define-key 'normal 'global (kbd "<leader>b") 'ivy-switch-buffer)
(evil-define-key 'normal 'global (kbd "<leader>f") 'counsel-find-file)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Setup Ivy
(progn
  (unless (package-installed-p 'counsel)
    (package-install 'counsel))
  (require 'counsel)
  (counsel-mode 1)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  ;; (global-set-key (kbd "K f") 'counsel-describe-function)
  ;; (global-set-key (kbd "K v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "K l") 'counsel-find-library)
  ;; (global-set-key (kbd "K i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "K u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "K j") 'counsel-set-variable)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view))

;; Themeing
;; todo
  (unless (package-installed-p 'badger-theme)
    (package-install 'badger-theme))
(load-theme 'badger t)
;(set-fringe-mode 10) ; give some breathing room
(set-cursor-color "#dc322f")

(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

(setq left-margin-width 26)
(setq right-margin-width 26)

;; Org Stuff
(add-hook 'org-mode-hook 'turn-on-flyspell) ;spell checking
(setq org-directory "~/Documents/org")
(setq org-agenda-files '("today.org" "refile.org"
			 "work.org" "Practices.org"))
(evil-define-key 'normal 'global (kbd "gA") 'org-agenda)
(if (display-graphic-p)
    (progn (unless (package-installed-p 'org-bullets)
	     (package-install 'org-bullets))
	   (require 'org-bullets)
	   (add-hook 'org-mode-hook #'org-bullets-mode)))
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
;; Make sure org-indent face is available
(require 'org-indent)

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
(unless (package-installed-p 'helpful)
  (package-install 'helpful))
(unless (package-installed-p 'company)
  (package-install 'company))
(require 'company)
(company-mode 1)
(unless (package-installed-p 'typo-suggest)
  (package-install 'typo-suggest))
(add-hook 'org-mode-hook 'typo-suggest-company-mode)
(add-hook 'mail-mode-hook 'typo-suggest-company-mode)

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

(unless (package-installed-p 'fennel-mode)
  (package-install 'fennel-mode))
(require 'fennel-mode)

(unless (package-installed-p 'racket-mode)
  (package-install 'racket-mode))
(require 'racket-mode)
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

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(add-to-list 'auto-mode-alist '("^/tmp/neomutt.*\\'" . mail-mode))
(add-hook 'mail-mode-hook 'flyspell-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("43f03c7bf52ec64cdf9f2c5956852be18c69b41c38ab5525d0bedfbd73619b6a" default))
 '(org-agenda-files '("~/Documents/org/today.org"))
 '(package-selected-packages
   '(typo-suggest type-suggest company helpful racket-mode fennel-mode undo-fu undo-fu-session org-bullets evil-collection ivy evil-lion evil-surround evil-commentary evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
