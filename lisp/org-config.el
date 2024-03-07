;;; ORG  -*- lexical-binding: t; -*-
(setq org-directory
      (cond
       ((eq system-type 'android) "/storage/emulated/0/Dropbox/Documents/org")
       (my/my-system "~/.local/Dropbox/Documents/org")
       (t (default-value 'org-directory))))

(use-package appt
  :demand t
  :custom
  ((appt-announce-method 'appt-persistant-message-announce)
   (appt-message-warning-time 15)
   (appt-display-duration 360)
   (appt-disp-window-function #'my/appt-display)
   (appt-delete-window-function #'ignore)
   )
  :init
  (defun my/appt-display (min-to-app _new-time appt-msg)
    (message "appt called")
    (require 'notifications)
    (funcall (if  (eq system-type 'android)
                 'android-notifications-notify
               'notifications-notify)
             :title (pcase min-to-app
                      ("0" "Appointment Is Starting")
                      ("1" "Appointment in 1 Minute")
                      (min (concat " Appointment in " min " Minutes"))
                      )
             :body appt-msg
             :urgency (if (< (string-to-number min-to-app) 3)
                          'critical
                        'normal)
             :actions '("Open" "Open this appointment")
             :on-action (lambda (id key) (org-agenda 'd))
             :replace-id (md5 appt-msg)))
  (appt-activate +1)
  (org-agenda-to-appt)
  (defvar appt-update-org-timer
    (run-with-idle-timer 60 60 #'org-agenda-to-appt)
    "Timer used to "))

;;; ORG
(use-package org
  :pin nongnu
  :hook ((org-mode . (lambda () (setq indent-tabs-mode nil)))
         (org-mode . (lambda ()  (setq-local electric-pair-inhibit-predicate
                                             `(lambda (c)
                                                (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))
  :bind (("C-c l" . org-store-link)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :repeat-map org-mode-repeatmap
         ("n"     . org-next-visible-heading)
         ("p"     . org-previous-visible-heading)
         ("<tab>" . org-cycle)
         ("u"     . outline-up-heading)
         ("f"     . org-forward-heading-same-level)
         ("b"     . org-backward-heading-same-level)
         ("d"     . outline-down-heading)
         ("M-f"   . org-next-block))
  :config
  (setq my/org-latex-scale 1.75)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale my/org-latex-scale))
;;;; Archive Completed Tasks
  (defun my/org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (org-map-entries 'org-archive-subtree "/CANCELLED" 'file))
;;;; Better defaults
  (setopt org-startup-indented t
          org-ellipsis " ▾"
          org-hide-emphasis-markers t
          org-pretty-entities t
          ;; C-e binding is pretty annoying to me
          org-special-ctrl-a/e '(t . nil)
          org-special-ctrl-k t
          org-src-fontify-natively t
          org-fontify-whole-heading-line t
          org-fontify-quote-and-verse-blocks t
          org-edit-src-content-indentation 2
          org-hide-block-startup nil
          org-src-tab-acts-natively t
          org-src-preserve-indentation nil
          org-startup-folded t
          org-cycle-separator-lines 2
          org-hide-leading-stars t
          org-highlight-latex-and-related '(native)
          org-goto-auto-isearch nil
          )
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "BACKLOG(b)" "ACTIVE(a)"
                    "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
                    "|" "DELEGATED(D)" "CANCELLED(c)")))
;;;; Agenda Files
  (when (file-directory-p org-directory)
    (setq org-agenda-files
          (seq-filter (lambda (x) (not (string-match "\\(completed.org\\|gcal.org\\)" x)))
                      (directory-files-recursively org-directory "\\.org$"))))
;;;; School notes
  ;; (let ((school-notes "~/.local/Dropbox/DropsyncFiles/vimwiki/School"))
  ;;   (when (file-directory-p school-notes)
  ;;     (setq org-agenda-files
  ;;           (append org-agenda-files
  ;;                   (directory-files-recursively school-notes "\\.org$")))))

  ;; (load (locate-user-emacs-file
  ;;        "lisp/org-conflict.el"))
  ;; (defun my/conflict-checker ()
  ;;   (interactive)
  ;;   (save-excursion
  ;;     (cl-loop for ts = (search-forward-regexp org-stamp-time-of-day-regexp nil t)
  ;;              while ts
  ;;              do (progn
  ;;                   (backward-char)
  ;;                   (call-interactively 'org-conflict-conflict-p)))))
  ;; (keymap-set org-mode-map "C-c C" 'org-conflict-conflict-p)
;;;; Clocking
  (setq org-clock-idle-time 15)
  (when-let ((idle-checker (executable-find "xprintidle")))
    (setq org-clock-x11idle-program-name idle-checker))
;;;; Refile targets
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(defun project-toggle-notes ()
  (interactive)
  ;; if org mode find the project associated with heading/file
  ;;   if no project found prompt for one
  ;; else look up alist for the current project
  (setq associated-projects-notes
        (list (cons  "~/.emacs.d/" (lambda ()
                                     (org-ql-select (directory-files org-directory t ".*.org") '(tags "gifts"))
                                     ))))
  ( (project-root (project-current)))
  (org-ql )
  )

(use-package org-agenda
  :bind (("C-c a" . org-agenda))
  :commands org-agenda
  :custom (;; make C-c a s work like Google
           (org-agenda-search-view-always-boolean t)
           (org-agenda-timegrid-use-ampm t)
           (org-agenda-time-grid
            '((daily today require-timed remove-match)
              (800 830 1000 1030 1200 1230 1400 1430 1600 1630 1700 1730 1800 1830 2000 )
              "......" "────────────────"))
           (org-agenda-current-time-string
            "← now ─────────────────")
           ;; Show effort estimates in agenda
           (org-agenda-columns-add-appointments-to-effort-sum t)
           ;; Agenda Views
           (org-agenda-custom-commands
            '(("d" "Today's Tasks"
               ((agenda "" ((org-agenda-span 1)
                            (org-agenda-overriding-header "Today's Tasks")
                            ))))
              ("." "Todays Agenda"
               ((agenda "" ((org-agenda-span 1)
                            (org-agenda-skip-deadline-prewarning-if-scheduled t)))))
              ("n" "Next Tasks"
               ((todo "NEXT"
                      ((org-agenda-overriding-header "Next Tasks")))))

              ("W" "Work Tasks" tags-todo "+work")

              ;; Low-effort next actions
              ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
               ((org-agenda-overriding-header "Low Effort Tasks")
                (org-agenda-max-todos 20)
                (org-agenda-files org-agenda-files)))))))

(use-package ob
  :custom ((org-confirm-babel-evaluate nil)
           (org-babel-clojure-backend 'cider)
           (org-babel-lisp-eval-fn #'sly-eval))
  :config
  ;;;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sqlite . t) (haskell . t) (emacs-lisp . t) (shell . t) (python . t)
     (C . t) (lua . t) (dot . t) (java . t)
     (lisp . t) (clojure . t) (scheme . t)
     (forth . t)
     (typescript . t))))
(use-package ob-typescript :ensure t :after ob)

(use-package ox
  :custom ((org-export-backends '(md org ascii html icalendar latex odt rss))
           (org-export-with-toc nil))
  :config
  ;; For exporting to markdown
  (require 'ox-md)
  (require 'ox-org))
(use-package ox-pandoc
  :when (executable-find "pandoc")
  :ensure t
  :after ox)
(use-package ox-rss :ensure t :after ox)
(use-package ox-gfm :ensure t :after ox
  :config
  (push 'gfm org-export-backends))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  :after org
  :init
  (when (file-directory-p org-directory)
    (setq org-default-notes-file (concat org-directory "/refile.org"))
    (setq org-capture-templates
          '(("t" "Todo" entry (file (lambda () (concat org-directory "/refile.org")))
             "* TODO %?\nDEADLINE: %T\n %i\n %a")
            ("m" "movie")
            ("mt" "movie" entry
             (file+headline (lambda () (concat org-directory "/mylife.org")) "Movies to Watch")
             "mw" "movie" entry
             (file+headline (lambda () (concat org-directory "/mylife.org")) "Movies Watched")
             "* %?\n")
            ("v" "Video Idea" entry
             (file+olp (lambda () (concat org-directory "/youtube.org"))
                       "YouTube" "Video Ideas")
             "* %?\n%? %a\n")

            ("g" "Gift")
            ("gs" "Gift For Seren" checkitem
             (file+headline (lambda () (concat org-directory "/archive.org")) "Gifts for Seren")
             nil
             :jump-to-captured t)
            ("gw" "Wish List" checkitem
             (file+headline (lambda () (concat org-directory "/site/wishlist.org")) "Wish list")
             nil
             :jump-to-captured t)

            ("u" "Update Current Clocked Heading" text
             (clock)
             "hello world %?"
             :unnarrowed t)

            ("k" "Knowledge")
            ("kc" "Cool Thing" entry
             (file+olp (lambda () (concat org-directory "/archive.org")) "Cool Projects")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("kk" "Thing I Learned" entry
             (file+olp (lambda () (concat org-directory "/archive.org")) "Knowledge")
             "* %? %^g\nEntered on %U\n  %i\n  %a")
            ("ki" "Ideas" entry
             (file+olp (lambda () (concat org-directory "/archive.org")) "Ideas")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("kT" "Thoughts" entry
             (file+olp (lambda () (concat org-directory "/archive.org")) "Thoughts")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("kw" "Word I learnt" entry
             (file+olp (lambda () (concat org-directory "/archive.org")) "Words")
             "* %?\nEntered on %U\n  %i\n  %a")

            ("s" "Scheduled Event")
            ("se" "Errand" entry (file (lambda () (concat org-directory "/refile.org")))
             "* TODO %? :errand\nDEADLINE: %T\n  %a")
            ("sm" "Meeting" entry
             (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
             "* Meeting with %? :MEETING:\nSCHEDULED: %T\n:PROPERTIES:\n:LOCATION: %^{location|Anywhere|Home|Work|School|FGPC}\n:END:")
            ("sE" "Event" entry
             (file+headline (lambda () (concat org-directory "/Work.org"))
                            "Events")
             "* Go to the %?\nSCHEDULED: %T\n\n:PROPERTIES:\n:LOCATION: %^{location|Anywhere|Home|Work|School}\n:END:")
            ("st" "Time Block" entry
             (file+headline (lambda () (concat org-directory "/Work.org"))
                            "Time Blocks")
             "* Work On %?\nSCHEDULED: %T\n")

            ;; Email Stuff
            ("e" "Email Workflow")
            ("ef" "Follow Up" entry
             (file+olp (lambda () (concat org-directory "/Work.org")) "Follow Up")
             "* TODO Follow up with %:fromname on %a\n SCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i")
            ("er" "Read Later" entry
             (file+olp (lambda () (concat org-directory "/Work.org")) "Read Later")
             "* TODO Read %:subject\n SCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n%i"))))
  (setopt org-capture-templates-contexts
          ;; I repeat "m" since org mode only supports this format
          '(("m" "m" ((in-mode . "mu4e-view-mode")
                      (in-mode . "mu4e-compose-mode"))))))

;;; 3rd party functionality
(use-package org-src-context
  :after org
  :init
  (unless (package-installed-p 'org-src-context)
    (package-vc-install "https://github.com/karthink/org-src-context"))
  :config
  (org-src-context-mode t))
(use-package org-contrib :ensure t :after org)
(use-package org-appear :after org
  :ensure t
  :custom
  (org-hide-emphasis-markers t)
  (org-appear-autolinks t)
  (org-appear-inside-latex t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

;; (use-package org-timeline
;;   :ensure t
;;   :after org-agenda
;;   :config
;;   (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

;;;; Drag And Drop
(use-package org-download
  :ensure t
  :bind (:map org-mode-map
              ("C-c i" . org-download-screenshot))
  :hook ((org-mode dired-mode) . org-download-enable)
  :init
  (setq-default org-download-screenshot-method "gnome-screenshot -a -f %s")
  (setq-default org-download-image-dir "./pic"))

;;;; Better Looking Bullets
(use-package org-modern
  :ensure t
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom ((org-modern-todo t)
           (org-modern-table nil)
           (org-modern-variable-pitch nil)
           (org-modern-block-fringe nil))
  :commands (org-modern-mode org-modern-agenda)
  :init (global-org-modern-mode))

;;;; Templates
(use-package org-tempo
  :ensure nil
  :after org
  :config
  (let ((templates '(("sh"  . "src sh")
                     ("el"  . "src emacs-lisp")
                     ("vim" . "src vim")
                     ("cpp" . "src C++ :includes <iostream>  :namespaces std"))))
    (dolist (template templates)
      (push template org-structure-template-alist))))

(use-package org-noter
  :ensure t
  :commands (org-noter)
  :after (pdf-tools)
  :init
  (setq org-noter-notes-search-path '("~/Documents/org/")))

(use-package org-transclusion
  :after org
  :bind ("C-c t" . org-transclusion-add))

(use-package org-protocol :ensure nil :after org)

;; Org Agenda Notifications
;; (use-package org-yaap
;;   :ensure nil
;;   :after org
;;   :defer 5
;;   :quelpa (org-yaap :fetcher gitlab :repo "tygrdev/org-yaap")
;;   :custom
;;   (org-yaap-altert-severity 'critical)
;;   (org-yaap-altert-before 10)
;;   :config
;;   (org-yaap-mode 1)
;;   (org-yaap-daemon-start))

(use-package my/backlinks :ensure nil :no-require t
  :bind (
         :map org-mode-map
         ("M-," . org-mark-ring-goto)
         ("C-c SPC" . consult-org-mark)
         ("C-c C-SPC" . peek-org-mark-ring))
  :custom
  (org-mark-ring-length 250)
  :init
  (defun consult-org-mark (&optional markers)
    (interactive)
    (consult--read (consult--global-mark-candidates (cl-loop for m in org-mark-ring
						             for i from 0 to 3
						             collect m))
		   :prompt "Go to global mark: "
		   ;; Despite `consult-global-mark' formatting the candidates in grep-like
		   ;; style, we are not using the `consult-grep' category, since the candidates
		   ;; have location markers attached.
		   :category 'consult-location
		   :sort nil
		   :require-match t
		   :lookup #'consult--lookup-location
		   :history '(:input consult--line-history)
		   :add-history (thing-at-point 'symbol)
		   :state (consult--jump-state)))
  (defun peek-org-mark-ring ()
    (interactive)
    (with-current-buffer (get-buffer-create "*Org Mark Ring*")
      (unless (eql major-mode #'fundamental-mode)
        (fundamental-mode))
      (read-only-mode -1)
      (erase-buffer)
      (dolist (b (cl-loop with seen = nil
                          for m in org-mark-ring
                          while (not (find m seen))
                          do (push m seen)
                          collect  m))
        (when-let* ((buf (marker-buffer b))
                    (name (buffer-name buf))
                    (pos  (marker-position b)))
          (insert-button (buffer-name (marker-buffer b)) 'face 'button
                         'keymap (let ((map (make-sparse-keymap))
                                       (jump (lambda ()
                                               (interactive)
                                               (with-current-buffer buf
                                                 (goto-char pos))
                                               (switch-to-buffer buf)
                                               (message "clicked"))))
                                   (define-key map (kbd "<mouse-1>") jump)
                                   (define-key map (kbd "RET") jump)
                                   map)
                         'mouse-face 'highlight
                         'help-echo "bad key, click to repalce")
          (insert "\n")))
      (read-only-mode +1)
      )
    (switch-to-buffer-other-window "*Org Mark Ring*")))

;; For creating svg art in org files
(use-package edraw
  :ensure nil
  :quelpa (edraw :fetcher github :repo "misohena/el-easydraw")
  :after org
  :config
  (with-eval-after-load 'org
    (require 'edraw-org)
    (edraw-org-setup-default)))
