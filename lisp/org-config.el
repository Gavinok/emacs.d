;;; ORG
(if my/is-termux
    (setq org-directory "~/storage/shared/Dropbox/Documents/org")
  (setq org-directory "~/Documents/org"))

;;; ORG
(use-package org-contrib
  :after org
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package org
  :pin nongnu
  ;; :ensure org-contrib
  :commands (org-capture org-agenda)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (setq my/org-latex-scale 1.75)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale my/org-latex-scale))
  (add-hook 'org-mode-hook (lambda () (setq indent-tabs-mode nil)))
;;;; Archive Completed Tasks
  (defun my-org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (org-map-entries 'org-archive-subtree "/CANCELLED" 'file))
;;;; Better defaults
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-special-ctrl-a/e '(t . nil) ; C-e binding is pretty annoying to me
        org-special-ctrl-k t
        org-src-fontify-natively t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-hide-leading-stars t
        org-export-backends '(markdown ascii html icalendar latex odt)
        org-export-with-toc nil
        org-agenda-timegrid-use-ampm t
        org-highlight-latex-and-related '(native)
        org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 830 1000 1030 1200 1230 1400 1430 1600 1630 1700 1730 1800 1830 2000 )
          "......" "────────────────")
        org-agenda-current-time-string
        "← now ─────────────────")
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "BACKLOG(b)" "ACTIVE(a)"
                    "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
                    "|" "DELEGATED(D)" "CANCELLED(c)")))
;;;; Babel
  (setq org-babel-lisp-eval-fn #'sly-eval)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t) (emacs-lisp . t) (shell . t)
     (C . t) (lua . t) (dot . t) (java . t)
     (lisp . t) (clojure . t)))
  (use-package ob-async
    :ensure t)
  (use-package ob-rust
    :ensure t)
  (use-package ob-racket
    :ensure nil
    :quelpa (ob-racket :fetcher github :repo "hasu/emacs-ob-racket")
    :after org
    :config
    ;; Enable Racket in Org-mode Babel
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((racket . t))))

  (setq org-confirm-babel-evaluate nil)
;;;; Agenda Views
  ;; Show effort estimates in agenda
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (when my/my-system
    (let ((school-notes "~/.local/Dropbox/DropsyncFiles/vimwiki/School"))
      (setq org-agenda-files (seq-filter (lambda (x) (not (string-match "completed.org" x)))
                                         (directory-files-recursively org-directory "\\.org$")))
      (if (file-directory-p school-notes)
          (setq org-agenda-files (append org-agenda-files
                                         (directory-files-recursively school-notes "\\.org$"))))))
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
  (when my/my-system
    (setq org-default-notes-file (concat org-directory "/refile.org"))
    (setq org-capture-templates
          '(("t" "Todo" entry (file (lambda () (concat org-directory "/refile.org")))
             "* TODO %?\nDEADLINE: %T\n %i\n %a")
            ("e" "Errand" entry (file (lambda () (concat org-directory "/refile.org")))
             "* TODO %? :errand\nDEADLINE: %T\n  %a")
            ("c" "Cool Thing" entry (file (lambda () (concat org-directory "/refile.org")))
             "* %?\nEntered on %U\n  %i\n  %a")
            ("C" "Coops" entry (file+olp (lambda () (concat org-directory "/refile.org")) "COOP")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("k" "Knowledge" entry (file+olp (lambda () (concat org-directory "/archive.org")) "Knowledge")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("s" "Scheduled Event")
            ("sm" "Meeting" entry (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
             "* Meeting with  %?\nSCHEDULED: %T\n")
            ("se" "Event" entry (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
             "* Meeting with  %?\nSCHEDULED: %T\n")
            ("M" "movie" entry (file+headline (lambda () (concat org-directory "/mylife.org")) "Movies to Watch")
             "* %?\n")
            ("v" "Video Idea" entry (file+olp (lambda () (concat org-directory "/youtube.org"))
                                              "YouTube" "Video Ideas")
             "* %?\n%? %a\n")
            ;; Email Stuff
            ("m" "Email Workflow")
            ("mf" "Follow Up" entry (file+olp (lambda () (concat org-directory "/Work.org")) "Follow Up")
             "* TODO Follow up with %:fromname on %a\n SCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i")
            ("mr" "Read Later" entry (file+olp (lambda () (concat org-directory "/Work.org")) "Read Later")
             "* TODO Read %:subject\n SCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n%i"))))
;;;; Clocking
  (setq org-clock-idle-time 15)
  (setq org-clock-x11idle-program-name "xprintidle")

;;;; Refile targets
  (setq org-refile-targets
        '(("Work.org"    :maxlevel . 3)
          ("archive.org" :maxlevel . 3)
          ("mylife.org"  :maxlevel . 3)
          ("youtube.org" :maxlevel . 3)
          ("today.org"   :maxlevel . 3)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
;;;; Font Sizes
  ;; (dolist (face '((org-level-1 . 1.0)
  ;;                 (org-level-2 . 1.0)
  ;;                 (org-level-3 . 1.0)
  ;;                 (org-level-4 . 1.0)))
  ;;   (set-face-attribute (car face) nil :family "PragmataPro Mono"
  ;;                       :weight 'normal
  ;;                       :height (cdr face)))
  )

(use-package org-timeline
  :after org
  :config
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

;;;; Drag And Drop
(use-package org-download
  :bind ("C-c i" . org-download-screenshot)
  :hook ((org-mode dired-mode) . org-download-enable)
  :init
  (setq-default org-download-screenshot-method "gnome-screenshot -a -f %s")
  (setq-default org-download-image-dir "./pic"))

;; (use-package plain-org-wiki
;;   :bind ("C-c C-x C-m" . plain-org-wiki)
;;   :init
;;   (setq plain-org-wiki-directory (concat org-directory "/wiki")))

;;;; Better Looking Bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-modern
  :commands (org-modern-mode org-modern-agenda)
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq org-modern-todo nil
        org-modern-variable-pitch nil)
  (global-org-modern-mode))


;;;; Templates
(use-package org-tempo
  :ensure nil
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh"  . "src sh"))
  (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("vim"  . "src vim"))
  (add-to-list 'org-structure-template-alist '("cpp"  . "src C++ :includes <iostream>  :namespaces std")))

(use-package svg-tag-mode
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  (setq svg-tag-tags
        `(
          ;; Org tags
          (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
          
          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority 
                                              :beg 2 :end -1 :margin 0))))

          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))
          
          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


          ;; Citation of the form [cite:@Knuth:1984] 
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                     (svg-tag-make tag
                                                                   :end -1
                                                                   :crop-left t))))

          
          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

  (svg-tag-mode t))

(use-package org-transclusion
  :after org
  :bind ("C-c t" . org-transclusion-add))

(use-package org-protocol
  :ensure nil
  :after org)

;; Org Agenda Notifications
(use-package org-yaap
  :ensure nil
  :after org
  :quelpa (org-yaap :fetcher gitlab :repo "tygrdev/org-yaap")
  :custom
  (org-yaap-altert-severity 'critical)
  (org-yaap-altert-before 10)
  :config
  (org-yaap-mode 1)
  (org-yaap-daemon-start))

;; (add-hook 'after-init-hook (lambda (&rest args) (org-agenda-list 1)))
