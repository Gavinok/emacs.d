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

(defun gv/backward-whitespace ()
    "like forward-whitespace but backwards"
    (interactive)
    ;; emulate C-u
    (let ((current-prefix-arg -1))
      ;; invoke forward-whitespace interactively
      (call-interactively 'forward-whitespace)))

(defun xah-escape-quotes (@begin @end)
  "Replace ?\"? by ?\\\"? in current line or text selection.
See also: `xah-unescape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" "FIXEDCASE" "LITERAL")))))

(defun xah-unescape-quotes (@begin @end)
  "Replace  ?\\\"? by ?\"? in current line or text selection.
See also: `xah-escape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" "FIXEDCASE" "LITERAL")))))

;; Text to speach script integratoin
(bind-key (kbd "C-x C-M-;")  #'gv/read)
(defun gv/read ()
  "text to speech"
  (interactive)
  (async-shell-command
   (concat "tts.sh " (shell-quote-argument (x-get-clipboard)))))

;; (call-process "tts.sh" nil nil nil (x-get-clipboard))

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

;;; UNDO
;; Vim style undo not needed for emacs 28
(use-package undo-fu
  :init (global-unset-key (kbd "C-/"))
  :defer nil
  :bind (;; I hate it when I accidentl
         ("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))

(use-package undo-fu-session
  :after undo-fu
  :init (global-unset-key (kbd "C-/"))
  :bind (("C-/" . undo-only)
         ;; Uses emacs 28s
	 ;; ("C-?" . undo-redo)
	 ;; ("C-x /" . undo-redo)
         )
  :init
  (global-undo-fu-session-mode))

;;; General Key Bindings
(use-package crux
  :ensure t
  :bind (;; Remove whitespace when killing at the end of a line
	 ([remap kill-line] . crux-kill-and-join-forward)
	 ;; Since C-j is so similar
	 ("C-S-o" . crux-smart-open-line-above)
	 ("C-o" . crux-smart-open-line)
         ("M-k" . crux-kill-whole-line)
	 ))


(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :demand t
  :ensure t)

(use-package embrace
  :demand t
  :ensure t)

;; (setq selected-org-mode-map (make-sparse-keymap))
;; (use-package selected
;;   :ensure t
;;   :init (selected-global-mode)
;;   :bind (:map selected-keymap
;;               ("=" . er/expand-region)
;;               ("a" . move-beginning-of-line)
;;               ("e" . move-beginning-of-line)
;;               ("n" . next-line)
;;               ("p" . previous-line)
;;               ("w" . kill-region)
;;               ("d" . kill-region)
;;               ("k" . kill-region)
;;               ("g" . keyboard-quit)
;;               ("<escape>" . keyboard-quit)
;;               :map selected-org-mode-map
;;               ("*" . (lambda () (interactive)
;;                        (org-emphasize ?*)))))


(defun my/mark-whole-line ()
  "Mark the entire line around or in front of point."
  (interactive)
  (move-beginning-of-line nil)
  (set-mark (point))
  (move-end-of-line nil))

(use-package dot-mode
  :demand t
  :ensure t
  :bind (("C-.". dot-mode-execute))
  :config
  (dot-mode-on))

(use-package god-mode
  :ensure t
  :bind (("C-z" . god-local-mode)
         ("<escape>" . god-local-mode)
         :map god-local-mode-map
         (";" . repeat)
         ("v" . god-mode-to-ryo)
         ("i" . god-local-mode)
         ("S" . embrace-add))
  :config
  (defun god-mode-to-ryo (&optional arg)
    (interactive)
    (god-local-mode -1)
    (ryo-modal-mode 1))
  
  ;; Cursor Shape
  (defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if god-local-mode 'box 'bar)))

  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
   
  ;; (defun ryo-advice (arg)
  ;;   (if god-local-mode
  ;;       (ryo-modal-mode 1)
  ;;     (ryo-modal-mode -1)))
  )

(use-package ryo-modal
  :ensure t
  :demand t
  :commands ryo-modal-mode
  :bind (("C-c c" . ryo-modal-mode))
  :config
  (ryo-modal-keys
   (:norepeat t)
   ;; ("i" ryo-modal-mode)
   ;; ("." ryo-modal-repeat)
   ;; ("n" next-line)
   ;; ("p" previous-line)
   ;; ;; Crux Stuff
   ;; ("e" move-end-of-line)
   ;; ("a" move-beginning-of-line)
   ;; ;; Boon stuff
   ;; ("f" forward-char)
   ;; ("b" backward-char)
   ;; ("SPC" set-mark-command)
   ;; ("/"  undo-fu-only-undo)
   ;; ("?"  undo-fu-only-redo)
   ("<escape>" ryo-modal-mode))
  
  (ryo-modal-keys
   ("k" kill-line)
   ("dd" kill-whole-line)
   ("D" kill-line)
   ("cc" my/mark-whole-line :then '(kill-region) :exit t)
   ("C" kill-line :exit t)
   ("s" embrace-add)
   ("cs" embrace-change)
   ("ds" embrace-delete))
  
  (let ((text-objects
         '(("iw" er/mark-word :name "Word")
           ("w" er/mark-word :name "Word")
           ("is" er/mark-symbol :name "symbol")
           ("as" er/mark-symbol-with-prefix :name "symbol")
           ("il" my/mark-whole-line :name "line")
           ;; ("at" er/mark-outer-tag)
           ("af" er/mark-outside-pairs)
           ("a(" er/mark-outside-pairs)
           ("a)" er/mark-outside-pairs)
           ("if" er/mark-inside-pairs)
           ("i(" er/mark-inside-pairs)
           ("i)" er/mark-inside-pairs)
           ("iq" er/mark-inside-quotes)
           ("aq" er/mark-outside-quotes)
           ("f" er/mark-defun)
           ("ip" er/mark-text-paragraph :name "Paragraphs")
           ("ap" mark-paragraph :name "Paragraphs")
           ;; comments
           ("ic" er/mark-comment :name "Comment")
           ("ac" er/mark-comment :name "Comment"))))
    (eval `(ryo-modal-keys
            ("<space>" ,text-objects :exit t)
            ("d" ,text-objects :then '(kill-region (lambda (&optional arg) (interactive)
                                                     (god-local-mode 1))) :exit t)
            ("c" ,text-objects :then '(kill-region (lambda (&optional arg) (interactive)
                                                     (god-local-mode -1))) :exit t)))))

;; git clone https://github.com/thblt/divine ~/.emacs.d/site-lisp/divine
;; (use-package divine
;;   :load-path "~/.emacs.d/site-lisp/divine"
;;   ;; :init (divine-global-mode)
;;   )


;;; Modal Bindings

;; (use-package evil
;;   :init
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-Y-yank-to-eol t)
;;   (setq evil-split-window-below t)
;;   (setq evil-split-window-right t)
;;   (setq evil-undo-system 'undo-fu)
;;   :config
;;   (evil-mode 1)
;;   (evil-set-leader 'normal " ")
;;   (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;   (evil-define-key 'normal 'global (kbd "<leader>t") 'capitalize-dwim)
;;   (evil-define-key 'visual 'global (kbd "<leader>t") 'capitalize-dwim)
;;   ;; Better lisp bindings
;;   (evil-define-key 'normal   'global (kbd "(")         'evil-previous-open-paren)
;;   (evil-define-key 'normal   'global (kbd ")")         'evil-next-close-paren)
;;   (evil-define-key 'operator 'global (kbd "(")         'evil-previous-open-paren)
;;   (evil-define-key 'operator 'global (kbd ")")         'evil-previous-close-paren)
;;  ;;;;; vim vinigar style
;;   (define-key  evil-normal-state-map (kbd "-") (lambda () (interactive)
;; 						 (dired "."))))
;; (use-package evil-collection
;;   :config
;;   (evil-collection-init)
;;   ;; Dired
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;                               "-" 'dired-up-directory))

;; ;; Enable Commentary
;; (use-package evil-commentary
;;   :bind (:map evil-normal-state-map
;; 	      ("gc" . evil-commentary)))

;; ;; Enable Surround
;; (use-package evil-surround
;;   :config
;;   (global-evil-surround-mode 1))

;; ;; Enable Lion
;; (use-package evil-lion
;;   :bind (:map evil-normal-state-map
;; 	      ("gl" . evil-lion-left)))

(use-package simple
  :ensure nil
  :bind (("M-SPC" . cycle-spacing)))

;; (use-package multiple-cursors
;;   :bind (("C-x <" . mc/mark-previous-like-this)
;; 	 ("C-x >" . mc/mark-next-like-this)
;; 	 ("C-x C->" . mc/mark-all-like-this)))

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
           ;; Used with clipmon makes emacs act as a
           ;; Clipboard Manager
           ("M-y" . consult-yank-pop))
    :custom
    (completion-in-region-function #'consult-completion-in-region)
    :config
    (add-hook 'completion-setup-hook #'hl-line-mode)
    (recentf-mode t))

;;;; Fuzzy Finding
  (use-package affe
    :bind (("C-c f" . affe-find)
	   ("C-c g" . affe-grep)
           ("C-c n" . gv/notegrep))
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

;; (use-package embark
;;   :ensure t
;;   :bind
;;   (("C-j" . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   ;; (add-to-list 'display-buffer-alist
;;   ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;   ;;                nil
;;   ;;                (window-parameters (mode-line-format . none))))

;;   ;; make it so I can actually see what is selected by embark
;;   (set-face-attribute 'embark-target nil :background "#666" :foreground "#ffffff"))
;;; REGION STUFF
;; (use-package expand-region
;;   :ensure t
;;   :bind ("M-j" . er/expand-region))

;; (use-package selected
;;   :bind (:map selected-keymap
;;               ("w" . kill-region)
;;               ("<tab>" . indent-region)
;;               :map selected-org-mode-map
;;               ("*" . (lambda () (interactive)
;;                        (org-emphasize ?*)))
;;               ("/" . (lambda () (interactive)
;;                        (org-emphasize ?/)))
;;               ("=" . (lambda () (interactive)
;;                        (org-emphasize ?=)))
;;               ("_" . (lambda () (interactive)
;;                        (org-emphasize ?_))))
;;   :init
;;   (setq selected-org-mode-map (make-sparse-keymap))

;;   (selected-global-mode t))
;;; THEMEING
(use-package ujelly-theme
  :config
  (load-theme 'ujelly t)
  (set-frame-parameter (selected-frame) 'alpha '(90 90))
  (add-to-list 'default-frame-alist '(alpha 90 90))
  (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
  (set-face-attribute 'default nil :background "#000" :foreground "#eee")
  (set-face-attribute 'mode-line nil
		      :box '(:line-width 10 :color "#000"))
  (set-face-attribute 'mode-line-inactive nil
		      :box '(:line-width 10 :color "#000"))
  (set-face-attribute 'mode-line nil
		      :background  "#0F0F0F")
(setq-default header-line-format nil))

;; (load-theme 'tsdh-light t)

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
        org-special-ctrl-k t
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
;;;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t) (emacs-lisp . t) (shell . t)
     (C . t) (lua . t) (dot . t) (java . t)))
  (use-package ob-rust
    :ensure t)
  (use-package ob-hy
    :ensure t)
  (use-package ob-elm
    :ensure t)
  (setq org-confirm-babel-evaluate nil)
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
          ("s" "Scheduled Event")
	  ("sm" "Meeting" entry (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
	   "* Meeting with  %?\nSCHEDULED: %T\n")
          ("se" "Event" entry (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
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
	   "* %?\nEntered on %U\n  %i\n  %a")
          ("m" "Email Workflow")
          ("mf" "Follow Up" entry (file+olp (lambda () (concat org-directory "/Work.org")) "Follow Up")
           "* TODO Follow up with %:fromname on %a\n SCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i")
          ("mr" "Read Later" entry (file+olp (lambda () (concat org-directory "/Work.org")) "Read Later")
           "* TODO Read %:subject\n SCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n%i")))
;;;; Clocking
  ;; (setq org-clock-continuously t)
  ;; (setq org-clock-auto-clockout-timer 2600)
  ;; (org-clock-auto-clockout-insinuate)
  (setq org-clock-idle-time 15)
  (setq org-clock-x11idle-program-name "xprintidle")
   
;;;; Refile targets
  (setq org-refile-targets
	'(("Work.org"    :maxlevel . 3)
	  ("archive.org" :maxlevel . 3)
	  ("mylife.org"  :maxlevel . 3)
          ("youtube.org"  :maxlevel . 3)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
;;;; Font Sizes
  (dolist (face '((org-level-1 . 1.05)
		  (org-level-2 . 1.05)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.05)))
    (set-face-attribute (car face) nil :font "RobotoMono Nerd Font" :weight 'medium :height (cdr face))))

(use-package org-notify
  :ensure nil
  :after org
  :config
  (org-notify-start)

  ;; (org-notify-add
  ;;  'default
  ;;  '(:time "10m" :period "5s" :duration 100 :actions -notify))
   ;; (org-notify-add
   ;; 'reminder
   ;; '(:time "10m" :period "5s" :duration 100 :actions -notify))
   )

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
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh"  . "src sh"))
  (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("vim"  . "src vim")))

;;;; Indentation
;; (use-package org-indent
;;   :ensure nil
;;   :defer t)

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
	company-minimum-prefix-length 1)
  ;; tab and go mode
  (company-tng-mode))

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
(use-package meghanada
  :bind (("M-."  . meghanada-jump-declaration)
         ("M-," . meghanada-back-jump))
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              ;; meghanada-mode on
              (meghanada-mode t)
              (flycheck-mode +1)
              (setq c-basic-offset 2)
              ;; use code format
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  (cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn"))))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;;; LSP
(use-package eglot
  :commands eglot
  :hook ((c-mode-common) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("ccls"))))

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

(use-package ffap
  :ensure nil
  :bind ("C-x f" . ffap)
  :init
  (setq find-file-visit-truename t))

;;; DEFAULTS
(use-package emacs
  :ensure nil
  :defer nil
  :bind (
         ("C-x C-d" . delete-pair)
         ;; ("M-?" . help-command)
         ;; ("C-h" . delete-backward-char)

         ("M-c" . capitalize-dwim)
         ("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim))
  :config

  (global-hl-line-mode t)
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
  :ensure nil
  :defer nil
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
	      ;; ("<tab>" . outline-cycle)
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
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; replace ugly ^L with harazontal lines
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

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

;;; EMAIL
(use-package mu4e
  :unless gv/is-termux
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :ensure nil
  :bind (("C-x M" . mu4e)
         ("C-x m" . mu4e-compose-new)
         ([remap mu4e~headers-jump-to-maildir] . my/jump-to-maildir))
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
  (defun my/jump-to-maildir ()
    (interactive)
    (let ((maildir (completing-read "Maildir: " (mu4e-get-maildirs))))
      (mu4e-headers-search (format "maildir:\"%s\"" maildir))))
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; This prevents saving the email to the Sent folder since gmail will do this for us on their end.
  (setq mu4e-sent-messages-behavior 'delete
        message-kill-buffer-on-exit t)

;;;; Attachment reminders
  (use-package message-attachment-reminder)
;;;; Org In Emails
  (use-package org-mime
    :config
    (setq org-mime-export-options '(:section-numbers nil
                                                     :with-author nil
                                                     :with-toc nil)))
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
    '(("sin" "Sincerely, \nGavin Jaeger-Freeborn" nil 1)))

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
		  (mu4e-drafts-folder               . "/[Gmail].Drafts")
		  (mu4e-sent-folder                 . "/[Gmail].Sent Mail")
		  (mu4e-refile-folder               . "/[Gmail].All Mail")
		  (mu4e-trash-folder                . "/[Gmail].Trash")))))
;;;; Contacts
  (setq mail-personal-alias-file  "~/.config/mutt/aliases")
  (setq mu4e-org-contacts-file  (concat org-directory "/contacts.org"))
  (add-to-list 'mu4e-headers-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
	       '("org-contact-add" . mu4e-action-add-org-contact) t)
;;;; Bookmarks
  (setq mu4e-bookmarks '((:name "Main INBOX"
			        :query "maildir:\"/INBOX\"" :key 115)
                         (:name "To Handle"
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

(use-package pinentry
  :ensure t)

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


;;; Extras
(use-package quelpa-use-package)
;; Don't forget to run M-x eaf-install-dependencies
(use-package eaf
  :unless gv/is-termux
  :bind (("C-c w" . gv/bm)
	("s-w" . gv/bm))
  ;; :demand t
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :init
  (use-package epc      :defer t :ensure t)
  (use-package ctable   :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s        :defer t :ensure t)
  ;; (setq browse-url-browser-function 'eaf-open-browser)
  :config
  (defun slurp (f)
    (with-temp-buffer
      (insert-file-contents f)
      (buffer-substring-no-properties
       (point-min)
       (point-max))))
  (defun gv/bm ()
    (interactive)
    (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/browser/")
    (require 'eaf-browser)

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
  (setq eaf-browser-continue-where-left-off t)
  (setq eaf-browser-dnefault-search-engine "duckduckgo")
  (setq eaf-browser-enable-adblocker "true")
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (dolist (keys '("i" "h" "j" "k" "l"
  ;;       	  "d" "f" "x" ","
  ;;       	  "-" "." "0" "1"
  ;;       	  "2" "=" "B" "F"
  ;;       	  "G" "H" "I" "J"
  ;;       	  "K" "L" "P" "T"
  ;;       	  "Y" "c" "d" "e"
  ;;       	  "f" "g" "m" "n"
  ;;       	  "o" "p" "r" "t"
  ;;       	  "u" "v" "x" "y"
  ;;       	  (eaf-bind-key nil key eaf-browser-keybinding))))
  )

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

;;; EXWM
(use-package exwm
  :unless gv/is-termux
  :init
  (setq exwm-workspace-number 4)
  :config
  (setq exwm-manage-force-tiling t)
 ;;;; Hooks
  (add-hook 'exwm-update-class-hook
	    (lambda ()
	      (exwm-workspace-rename-buffer exwm-class-name)))

    ;; (add-hook 'exwm-manage-finish-hook
    ;;         (lambda ()
    ;;           (when (and exwm-class-name
    ;;     		 (or (string= exwm-class-name "qutebrowser")
    ;;     		     (string= exwm-class-name "libreoffice-writer")
    ;;     		     (string= exwm-class-name "libreoffice-calc")
    ;;     		     (string= exwm-class-name "Google-chrome")
    ;;     		     (string= exwm-class-name "Brave-browser")))
    ;;     	(exwm-input-set-local-simulation-keys nil))))

    (add-hook 'exwm-update-title-hook
	    (lambda ()
	      (pcase exwm-class-name
		("qutebrowser" (exwm-workspace-rename-buffer (format "%s" exwm-title)))
		("libreoffice-writer" (exwm-workspace-rename-buffer (format "Writer: %s" exwm-title)))
		("libreoffice-calc" (exwm-workspace-rename-buffer (format "Calc: %s" exwm-title)))
		("St" (exwm-workspace-rename-buffer (format "%s" exwm-title))))))
    ;; Hide the modeline on all X windows
    (add-hook 'exwm-floating-setup-hook
              (lambda ()
                (exwm-layout-hide-mode-line)))
;;;; Funcs
  (defun gv/switch-to-scratch-and-back ()
    "Toggle between *scratch* buffer and the current buffer.
     If the *scratch* buffer does not exist, create it."
    (interactive)
    (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
      (if (equal (current-buffer) scratch-buffer-name)
          (switch-to-buffer (other-buffer))
        (switch-to-buffer scratch-buffer-name (lisp-interaction-mode)))))

  ;;Just disallow killing of scratch buffer
  (defun gv/unkillable-scratch-buffer ()
    (if (equal (buffer-name (current-buffer)) "*scratch*")
	(progn
	  (delete-region (point-min) (point-max))
	  nil)
      t))

  (add-hook 'kill-buffer-query-functions 'gv/unkillable-scratch-buffer)
;;;; Global Key Bindings
  (setq exwm-input-global-keys
	`(([?\s-h] . windmove-left)
	  ([?\s-l] . windmove-right)
	  ;; Window Managment
	  (,(kbd "<s-tab>") . other-window)
	  ([?\s-v] . crux-swap-windows)
	  ;; ([?\s-v] . crux-transpose-windows)
	  ([?\s-o] . gv/switch-to-scratch-and-back)
	  ([?\s-f] . exwm-layout-set-fullscreen)
	  ([?\s-c] . inferior-octave)
	  ([?\s-C] . kill-this-buffer)

	  ;; tile exwm
	  ([?\s-t] . exwm-reset)

	  ;; open a terminal
	  (,(kbd "<s-return>") . vterm)
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
	  ([?\s-=] . (lambda ()
		       (interactive)
		       (start-process-shell-command "Connections" nil
						    "menu_connection_manager.sh")))
	  ([?\s-p] . (lambda ()
		       (interactive)
		       (start-process-shell-command "Clipmenu" nil "clipmenu")))
;;;; Workspaces
	  ([?\s-g] . exwm-workspace-switch)))
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (define-key exwm-mode-map (kbd "<s-escape>") 'exwm-input-release-keyboard)
                                        ;: Start in char-mode
  ;; (setq exwm-manage-configurations '((t char-mode t)))

  (fringe-mode 1)
;;;; Start EXWM
  (exwm-enable)
;;;; Start Programs For EXWM
  ;;   ;; (start-process-shell-command "blueman-applet" nil "blueman-applet")
  (start-process-shell-command "nm-applet" nil "nm-applet")
  (start-process-shell-command "kdeconnect-indicator " nil "kdeconnect-indicator")
  ;; ;;;; Window Divider
  ;;   (setq window-divider-default-right-width 3)
  ;;   (let ((color (face-background 'mode-line)))
  ;;     (dolist (face '(window-divider-first-pixel
  ;; 		    window-divider-last-pixel
  ;; 		    window-divider))
  ;;       (set-face-foreground face color)))

  ;;   (window-divider-mode 1)
  ;; ;;;; Mouse Settings
  ;;   :init (setq mouse-autoselect-window t
  ;; 	      focus-follows-mouse t)
  )
(use-package exwm-systemtray
  :ensure nil
  :after exwm
  :config
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 23))
(use-package exwm-randr
  :ensure nil
  :after exwm
  :defer t
  :config
  (setq exwm-randr-workspace-output-plist '(3 "HDMI2"))
  (add-hook 'exwm-randr-screen-change-hook
	    (lambda ()
	      (start-process-shell-command
	       "xrandr" nil "xrandr --output eDP1 --primary --auto --left-of HDMI2 --auto")))
  (exwm-randr-enable))

;; use emacs as a clipboard manager
(use-package clipmon
  :unless (and gv/is-termux (not (executable-find "clipmon")))
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
;;; Stuff To Ignore
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
