;;; ORG  -*- lexical-binding: t; -*-
(setq org-directory
      (cond
       ((eq system-type 'android) "/storage/emulated/0/Dropbox/Documents/org")
       (my/my-system "~/.local/Dropbox/Documents/org")
       (t (default-value 'org-directory))))

(add-hook 'after-init-hook 'my/quick-agenda)
(defvar my/notifications nil)
(defun get-last-note (msg)
  (alist-get (md5 msg) my/notifications nil nil 'equal))
(defun store-last-note (replace-id msg)
  (setf (alist-get (md5 msg) my/notifications nil nil 'equal) replace-id))
(use-package appt
  :demand t
  :defer 5
  :custom
  ((appt-announce-method 'appt-persistant-message-announce)
   (appt-message-warning-time 15)
   (appt-display-duration 360)
   (appt-disp-window-function #'my/appt-display)
   (appt-delete-window-function #'ignore))
  :init
  (defun my/appt-display (min-to-app _new-time appt-msg)
    (require 'notifications)
    ;; Close the last iteration of this notification
    (when-let ((repid (get-last-note appt-msg)))
      (notifications-close-notification repid))
    (let ((title  (pcase min-to-app
                    ("0" "Appointment Is Starting")
                    ("1" "Appointment in 1 Minute")
                    (min (concat " Appointment in " min " Minutes"))))
          (body appt-msg)
          (urgency (if (< (string-to-number min-to-app) 3)
                       'critical
                     'normal)))
      (store-last-note
       (funcall (if  (eq system-type 'android)
                    ;; support for android notifications
                    'android-notifications-notify
                  'notifications-notify)
                :title title
                :body body
                :urgency urgency
                :actions '("Open" "Open this appointment")
                :on-action (lambda (_id _key) (org-agenda 'd)))
       appt-msg)
      ;; Notify me on my phone
      (require 'plz)
      (plz 'post "https://ntfy.sh/gavin-emacs-notifications"
        :headers `(("Title"  . ,title)
                   ("Priority" . ,(if (eql urgency 'critical)
                                      "high"
                                    "default"))
                   ("Tags" . "calendar")
                   ("Markdown" . "yes"))
        :body body)))
  (appt-activate +1)
  (org-agenda-to-appt)
  (defvar appt-update-org-timer
    ;; wait till emacs is idle for 1 minute before processing org appointments
    (run-with-idle-timer 60 t #'org-agenda-to-appt)
    "Timer used to update appt to the current org events")
  )

;;; ORG
(use-package org
  :pin nongnu
  :hook ((org-mode . (lambda () (setq indent-tabs-mode nil)))
         ;; (org-mode . (lambda () (setq-local
         ;;                         electric-pair-inhibit-predicate
         ;;                         `(lambda (c)
         ;;                            (if (char-equal c ?<)
         ;;                                t
         ;;                              (,electric-pair-inhibit-predicate c))))))
         )
  :bind (("C-c i" . org-insert-link-global)
         ("C-c o" . org-open-at-point-global)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ("C-c d" . my/org-draw-on-code))
  :init
  (setq org-use-sub-superscripts '{})
  (setopt org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "BACKLOG(b)" "ACTIVE(a)"
                      "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
                      "|" "DELEGATED(D)" "CANCELLED(c)")))
  :config
  (defun my/org-draw-on-code ()
    (interactive)
    (save-excursion
      (unless (region-active-p)
        (org-mark-element))
      (narrow-to-region (region-beginning) (region-end))
      (org-latex-export-to-pdf nil nil t)
      (start-process-shell-command "rnote"
                                   nil
                                   (concat "rnote"
                                           " "
                                           (org-export-output-file-name ".pdf" nil)))
      (widen)))
  ;; Prevent editing text inside of folds cuz that is just silly
  (setq org-catch-invisible-edits 'error)
  (setopt org-use-speed-commands t)
  ;; Add support for using embedded calc with org math
  (with-eval-after-load 'calc
    (add-to-list 'calc-language-alist
                 '(org-mode . latex)))
  (setq my/org-latex-scale 1.75)
  ;; Less blurry latex preview
  (setq org-preview-latex-default-process 'dvisvgm)
  ;; Better colors for latex preview with dvisvgm
  (setq org-format-latex-options
        `(:foreground auto :background auto :scale ,my/org-latex-scale :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                      ("begin" "$1" "$" "$$" "\\(" "\\[")))


  ;; Ensure that latex previews are scaled as well as text
  (defun my/text-scale-adjust-latex-previews ()
    "Adjust the size of latex preview fragments when changing the
  buffer's text scale."
    (pcase major-mode
      ('latex-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'category)
                 'preview-overlay)
             (my/text-scale--resize-fragment ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'org-overlay-type)
                 'org-latex-overlay)
             (my/text-scale--resize-fragment ov))))))

  (defun my/text-scale--resize-fragment (ov)
    (overlay-put
     ov 'display
     (cons 'image
           (plist-put
            (cdr (overlay-get ov 'display))
            :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

  (add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)
;;;; Archive Completed Tasks
  (defun my/org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (org-map-entries 'org-archive-subtree "/CANCELLED" 'file))
;;;; Better defaults
  (setopt org-startup-indented t
          org-ellipsis ;; "⮧"
          " ▾"
          org-hide-emphasis-markers t
          org-pretty-entities t
          ;; C-e binding is pretty annoying to me
          org-special-ctrl-a/e '(t . nil)
          org-special-ctrl-k t
          org-src-fontify-natively t
          org-fontify-whole-heading-line t
          org-fontify-quote-and-verse-blocks t
          org-edit-src-content-indentation 0
          org-hide-block-startup nil
          org-src-tab-acts-natively t
          org-src-preserve-indentation nil
          org-startup-folded t
          org-cycle-separator-lines 2
          org-hide-leading-stars t
          org-highlight-latex-and-related '(native)
          org-goto-auto-isearch nil)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

;;;; Agenda Files
  (when (file-directory-p org-directory)
    (setq org-agenda-files
          (seq-filter (lambda (x) (not (string-match "completed.org" x)))
                      (directory-files-recursively org-directory "\\.org$"))))
;;;; Clocking
  (setq org-clock-idle-time 15)
  (when-let ((idle-checker (executable-find "xprintidle")))
    (setq org-clock-x11idle-program-name idle-checker))
;;;; Refile targets
  (setopt org-refile-targets '((org-agenda-files :maxlevel . 9)))
  (setopt org-refile-use-outline-path 'file) ; Let me refile to file as well as a heading
  (setq org-outline-path-complete-in-steps nil) ; give me all completions at once
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(use-package org-agenda
  :bind (("C-c a" . org-agenda)
         ("<f12>" . my/quick-agenda))
  :commands org-agenda
  :custom (;; make C-c a s work like Google
           (org-agenda-search-view-always-boolean nil)
           (org-agenda-timegrid-use-ampm t)

           ;; Stuff to skip in my agenda
           (org-agenda-skip-scheduled-if-done t)
           (org-agenda-skip-deadline-prewarning-if-scheduled t)

           ;; If an appointment does not have a duration assume it
           ;; will take 30 minutes
           (org-agenda-default-appointment-duration 30)
           (org-agenda-time-grid
            '((daily today require-timed remove-match)
              (700 800 830 1000 1030 1200 1230 1400 1430 1600 1630 1700 1730 1800 1830 2000 )
              "......" "────────────────"))
           (org-agenda-current-time-string
            "← now ─────────────────")
           ;; Show effort estimates in agenda
           (org-agenda-columns-add-appointments-to-effort-sum t)
           ;; Agenda Views
           (org-agenda-custom-commands
            `(("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("w" "Weekly Review"
               ((tags-todo "REFILE|ORGZLY_REFILE"
                           ((org-agenda-overriding-header "Tasks to Refile")))
                (agenda "" ((org-agenda-overriding-header "This Week's Events")))))
              ("y" "Youtube"
               ((tags-todo "TODO={WAIT\\|RESEARCHING\\|PLANNING}"
                           ((org-agenda-overriding-header "Backlog")
                            (org-agenda-max-entries 5)))
                (tags-todo "TODO={PLANNED\\|RECORDING\\|RECORDED\\|EDITING\\|RENDERING}+youtube"
                           ((org-agenda-overriding-header "Doing")))
                (tags-todo "TODO={THUMBNAIL\\|DESCRIPTION\\|UPLOADING\\|UNLISTED}+youtube"
                           ((org-agenda-overriding-header "Review")))
                (tags-todo "TODO={POSTED\\|CANCELLED}+youtube"
                           ((org-agenda-overriding-header "Done")))))
              ("k" "Kanban"
               ((tags-todo "TODO={TODO\\|BACKLOG}+work"
                           ((org-agenda-overriding-header "Backlog")))
                (tags-todo "TODO={NEXT\\|ACTIVE}+work"
                           ((org-agenda-overriding-header "Doing")))
                (tags "TODO={REVIEW\\|DELEGATED}+work"
                      ((org-agenda-overriding-header "Review")))
                (tags "TODO=DONE+work"
                      ((org-agenda-overriding-header "Done")))))
              ("q" "Life Tasks"
               ((tags-todo "REFILE|ORGZLY_REFILE"
                           ((org-agenda-overriding-header "Tasks to Refile")))
                (tags-todo "work-STYLE=\"habit\""
                           ((org-agenda-overriding-header "Work Tasks")))
                (agenda "" ((org-agenda-span
                             ;; Display week view on mondays
                             (if (my/monday-p)
                                 'week
                               1))
                            (org-habit-show-habits-only-for-today t)
                            (org-agenda-overriding-header
                             (if (my/monday-p)
                                 "This Week's Tasks"
                               "Today's Tasks"))))))
              ("d" "Today's Tasks"
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
                (org-agenda-files org-agenda-files)))))
           (org-deadline-warning-days 30))
  :config
  (add-to-list 'org-modules 'org-habit)
  (setopt org-habit-graph-column 50)

  (defun my/quick-agenda ()
    (interactive)
    (let ((org-agenda-window-setup 'current-window))
      (org-agenda nil "q")))

  (defun my/working-p ()
    "Determine if today is monday"
    (require 'org-clock)
    (let* ((d (calendar-current-date))
           (day (cl-second d))
           (month (cl-first d))
           (year (cl-third d))
           (hour-of-day (string-to-number (format-time-string "%H"))))
      (and
       ;; Workday
       (<= (org-day-of-week day month year) 5)
       ;; Working Hours
       (and (< 6 hour-of-day 17)))))
  (my/working-p)
  (defun my/monday-p ()
    "Determine if today is monday"
    (require 'org-clock)
    (let* ((d (calendar-current-date))
           (day (cl-second d))
           (month (cl-first d))
           (year (cl-third d)))
      (= (org-day-of-week day month year)
         1))))

(use-package ob
  :custom ((org-confirm-babel-evaluate nil)
           (org-babel-clojure-backend 'cider)
           (org-babel-lisp-eval-fn #'sly-eval)
           ;; Ensure we don't break indentation for languages like Haskell or Python
           (org-edit-src-content-indentation 0))
  :config
  ;;;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sqlite . t) (haskell . t) (emacs-lisp . t) (shell . t) (python . t)
     (C . t) (lua . t) (dot . t) (java . t)
     (lisp . t) (clojure . t) (scheme . t)
     (forth . t) (typescript . t) (R . t))))

(use-package ob-typescript :ensure t :after ob)

(use-package ox
  :custom ((org-export-backends '(md org ascii html icalendar latex odt rss))
           (org-export-with-toc nil))
  :config
  ;; For exporting to markdown
  (require 'ox-md)
  (require 'ox-org)
  ;; Make sure we get syntax highlighting in for pdf's
  )
(use-package engrave-faces
  :ensure t
  :after ox-latex
  :config
  (setopt org-latex-src-block-backend 'engraved))
(use-package ox-pandoc
  :when (executable-find "pandoc")
  :ensure t
  :after ox)
(use-package ox-rss :ensure t :after ox)
(use-package ox-gfm :ensure t :after ox
  :config
  (add-to-list 'org-export-backends 'gfm))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  :after org
  :init
  (when (file-directory-p org-directory)
    (setq org-default-notes-file (concat org-directory "/refile.org"))
    (setq org-capture-templates
          '(("t" "Todo" entry (file (lambda () (concat org-directory "/refile.org")))
             "* TODO %?\nDEADLINE: %T\n %i\n %a")
            ("m" "movie" entry
             (file+headline (lambda () (concat org-directory "/mylife.org")) "Movies to Watch"))
            ("v" "Video Idea" entry
             (file+olp (lambda () (concat org-directory "/youtube.org")) "YouTube" "Video Ideas")
             "* RESEARCHING %?\n %? %a\n")

            ("g" "Gift")
            ("gs" "Gift For Seren" checkitem
             (file+headline (lambda () (concat org-directory "/archive.org")) "Gifts for Seren")
             nil
             :jump-to-captured t)
            ("gw" "Wish List" checkitem
             (file+headline (lambda () (concat org-directory "/site/wishlist.org")) "Wish list")
             "- [ ] %a"
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

            ("s" "Schedule")
            ;; Not sure I even want to have this
            ;; ("sE" "Errand" entry (file (lambda () (concat org-directory "/refile.org")))
            ;;  "* TODO %? :errand\nDEADLINE: %T\n  %a")
            ("sm" "Meeting" entry
             (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
             "* Meeting with %? :MEETING:\nSCHEDULED: %T\n:PROPERTIES:\n:LOCATION: %^{location|Anywhere|Home|Work|School|FGPC}\n:END:")
            ("se" "Event" entry
             (file+headline (lambda () (concat org-directory "/mylife.org"))
                            "Events")
             "* Go to the %?\n%T\n\n:PROPERTIES:\n:LOCATION: %^{location|Anywhere|Home|Work|School}\n:END:")
            ("st" "Time Block" entry
             (file+headline (lambda () (concat org-directory "/Work.org"))
                            "Time Blocks")
             "* Work On %?\nSCHEDULED: %T\n")

            ;; Org Protocol
            ;; ("pc" "Cool Thing" entry
            ;;  (file+olp (lambda () (concat org-directory "/archive.org")) "Cool Projects")
            ;;  "* %^{Title}\n\n  %^g\nEntered on %U\n  Source: %u, %c\n\n  %i")
            ("p" "Thing I Learned" entry
             (file+olp (lambda () (concat org-directory "/archive.org")) "Knowledge")
             "* %^{Title}")

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
          '(("e" "e" ((in-mode . "mu4e-view-mode")
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

;;;; Does a better job at making tables look good than org modern
(use-package valign
  :ensure t
  :custom
  (defun my/enable-fancy-bars (&rest app)
    (valign-mode)
    (valign-fancy-bar t)
    (apply app))
  (advice-add #'org-latex-preview :after #'my/enable-fancy-bars))


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
  (defun consult-org-mark (&optional _markers)
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
                          while (not (cl-find m seen))
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
  :init
  (unless (package-installed-p 'edraw)
    (package-vc-install "https://github.com/misohena/el-easydraw"))
  :config
  (with-eval-after-load 'org
    (require 'edraw-org)
    (edraw-org-setup-default)))
