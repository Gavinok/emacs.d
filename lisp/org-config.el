;;; ORG
(if my/is-termux
    (setq org-directory "~/storage/shared/Dropbox/Documents/org")
  (setq org-directory "~/.local/Dropbox/Documents/org"))

;;; ORG
(use-package org
  :pin nongnu
  ;; :ensure org-contrib
  :commands (org-capture org-agenda)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
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
  (setq org-startup-indented t)
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
        ;; C-e binding is pretty annoying to me
        org-special-ctrl-a/e '(t . nil)
        org-special-ctrl-k t
        org-src-fontify-natively t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'fold
        org-cycle-separator-lines 2
        org-hide-leading-stars t
        org-export-backends '(markdown ascii html icalendar latex odt)
        org-export-with-toc nil
        org-highlight-latex-and-related '(native)
        org-goto-auto-isearch nil
        ;; make C-c a s work like googls
        org-agenda-search-view-always-boolean t
        org-agenda-timegrid-use-ampm t
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
  (use-package ob-typescript :demand t)
  (setq org-babel-lisp-eval-fn #'sly-eval)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t) (emacs-lisp . t) (shell . t) (python . t)
     (C . t) (lua . t) (dot . t) (java . t)
     (lisp . t) (clojure . t) (scheme . t)
     (forth . t)
     (typescript . t)))
  (setq org-confirm-babel-evaluate nil)
  ;;;; School notes
  (when my/my-system
    (let ((school-notes "~/.local/Dropbox/DropsyncFiles/vimwiki/School"))
      (setq org-agenda-files
            (seq-filter (lambda (x) (not (string-match "\\(completed.org\\|gcal.org\\)" x)))
                        (directory-files-recursively org-directory "\\.org$")))
      (if (file-directory-p school-notes)
          (setq org-agenda-files
                (append org-agenda-files
                        (directory-files-recursively school-notes "\\.org$"))))))
;;;; Clocking
  (setq org-clock-idle-time 15)
  (setq org-clock-x11idle-program-name "xprintidle")
;;;; Refile targets
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(use-package org-contrib :after org)

(use-package ob-config :ensure nil :no-require t
  :after org-contrib
  :config
  (setq org-babel-clojure-backend 'cider)
  ;; For exporting to markdown
  (require 'ox-md))

(use-package org-agenda-config :ensure nil :no-require t
  :after org
  :config
  ;; Show effort estimates in agenda
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  ;; Agenda Views
  (setq org-agenda-custom-commands
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
            (org-agenda-files org-agenda-files))))))

(use-package org-capture-config :ensure nil :no-require t
  :when my/my-system
  :after org
  :init
  (setq org-default-notes-file (concat org-directory "/refile.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file (lambda () (concat org-directory "/refile.org")))
           "* TODO %?\nDEADLINE: %T\n %i\n %a")
          ("M" "movie" entry
           (file+headline (lambda () (concat org-directory "/mylife.org")) "Movies to Watch")
           "* %?\n")
          ("v" "Video Idea" entry
           (file+olp (lambda () (concat org-directory "/youtube.org"))
                     "YouTube" "Video Ideas")
           "* %?\n%? %a\n")

          ("k" "Knowledge")
          ("kc" "Cool Thing" entry
           (file+olp (lambda () (concat org-directory "/archive.org")) "Cool Projects")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("kk" "Thing I Learned" entry
           (file+olp (lambda () (concat org-directory "/archive.org")) "Knowledge")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("ki" "Ideas" entry
           (file+olp (lambda () (concat org-directory "/archive.org")) "Ideas")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("kT" "Thoughts" entry
           (file+olp (lambda () (concat org-directory "/archive.org")) "Thoughts")
           "* %?\nEntered on %U\n  %i\n  %a")

          ("s" "Scheduled Event")
          ("se" "Errand" entry (file (lambda () (concat org-directory "/refile.org")))
           "* TODO %? :errand\nDEADLINE: %T\n  %a")
          ("sm" "Meeting" entry
           (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
           "* Meeting with  %? :MEETING:\nSCHEDULED: %T\n:PROPERTIES:\n:LOCATION: %^{location|Anywhere|Home|Work|School}\n:END:")
          ("sE" "Event" entry
           (file+headline (lambda () (concat org-directory "/Work.org"))
                          "Events")
           "* Go to the %?\nSCHEDULED: %T\n\n:PROPERTIES:\n:LOCATION: %^{location|Anywhere|Home|Work|School}\n:END:")
          ("st" "Time Block" entry
           (file+headline (lambda () (concat org-directory "/Work.org"))
                          "Time Blocks")
           "* Work On %?\nSCHEDULED: %T\n")

          ;; Email Stuff
          ("m" "Email Workflow")
          ("mf" "Follow Up" entry
           (file+olp (lambda () (concat org-directory "/Work.org")) "Follow Up")
           "* TODO Follow up with %:fromname on %a\n SCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i")
          ("mr" "Read Later" entry
           (file+olp (lambda () (concat org-directory "/Work.org")) "Read Later")
           "* TODO Read %:subject\n SCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n%i")))
  (setq org-capture-templates-contexts
        '(("m" ((in-mode . "mu4e-view-mode")
                (in-mode . "mu4e-compose-mode"))))))

(use-package org-timeline
  :commands org-agenda
  :init
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

;;;; Drag And Drop
(use-package org-download
  :bind ("C-c i" . org-download-screenshot)
  :hook ((org-mode dired-mode) . org-download-enable)
  :init
  (setq-default org-download-screenshot-method "gnome-screenshot -a -f %s")
  (setq-default org-download-image-dir "./pic"))

;;;; Better Looking Bullets
(use-package org-modern
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
  :after (pdf-tools)
  :init
  (setq org-noter-notes-search-path '("~/Documents/org/")))

(use-package org-transclusion
  :after org
  :bind ("C-c t" . org-transclusion-add))

(use-package org-protocol :ensure nil :after org)

;; Org Agenda Notifications
(use-package org-yaap
  :ensure nil
  :after org
  :defer 5
  :quelpa (org-yaap :fetcher gitlab :repo "tygrdev/org-yaap")
  :custom
  (org-yaap-altert-severity 'critical)
  (org-yaap-altert-before 10)
  :config
  (org-yaap-mode 1)
  (org-yaap-daemon-start))

(use-package my/backlinks :ensure nil :no-require t
  :bind (
         :map org-mode-map
         ("M-," . org-mark-ring-goto))
  :custom
  (org-mark-ring-length 250)
  :init
  (defun peek-mark-ring ()
    (interactive)
    (with-current-buffer (get-buffer-create "*Org Mark Ring*")
      (unless (eql major-mode #'org-mode)
        (org-mode))
      (erase-buffer)
      (mapcar (lambda (b)
                (insert b "\n"))
              (cl-loop
               with seen = nil
               for m in org-mark-ring
               while (not (find m seen))
               do (push m seen)
               collect (buffer-name (marker-buffer m)))))
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

;; (add-hook 'after-init-hook (lambda (&rest args) (org-agenda-List 1)))
;; (mapcar (lambda (selected)
;;           (plist-get
;;            (plist-get selected
;;                       'headline)
;;            :raw-value))
;;         (org-ql-select (directory-files-recursively org-directory "\\.org$")
;;           '(and (property "LOCATION"))))

(plist-get '(headline (1)) 'headline)
