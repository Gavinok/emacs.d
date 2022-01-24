;;; ORG
(if gv/is-termux
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
  :bind (("C-c y" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
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
        org-fontify-whole-heading-line t
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
  (use-package ob-hy
    :ensure t)
  (use-package ob-elm
    :ensure t)
  (setq org-confirm-babel-evaluate nil)
;;;; Agenda Views
  (when gv/my-system
    (setq org-agenda-files (seq-filter (lambda (x) (not (string-match "completed.org" x)))
                                     (directory-files-recursively org-directory "\\.org$"))))
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
  (when gv/my-system
    (setq org-default-notes-file (concat org-directory "/refile.org"))
    (setq org-capture-templates
          '(("P" "Protocol")
            ("Pi" "Important")
            ("Pit" "Today" entry (file+headline "~/Documents/Org/Agenda/notes.org" "Websites")
             "* TODO %:annotation \t:important:\n\tSCHEDULED:%(org-insert-time-stamp (org-read-date nil t \"\"))\n:PROPERTIES:\n:Effort: 1h\n:SCORE_ON_DONE: 30\n:END:\n  %i\n  %a")
            ("t" "Todo" entry (file (lambda () (concat org-directory "/refile.org")))
             "* TODO %?\nDEADLINE: %T\n  %a")
            ("M" "movie" entry (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
             "* Meeting with  %?\nSCHEDULED: %T\n")
            ("s" "Scheduled Event")
            ("sm" "Meeting" entry (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
             "* Meeting with  %?\nSCHEDULED: %T\n")
            ("se" "Event" entry (file+headline (lambda () (concat org-directory "/Work.org")) "Meetings")
             "* Meeting with  %?\nSCHEDULED: %T\n")
            ("v" "Video Idea" entry (file+olp (lambda () (concat org-directory "/youtube.org"))
                                              "YouTube" "Video Ideas")
             "* %?\n%? %a\n")
            ("c" "Cool Thing" entry (file (lambda () (concat org-directory "/refile.org")))
             "* %?\nEntered on %U\n  %i\n  %a")
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
          ("youtube.org"  :maxlevel . 3)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
;;;; Font Sizes
  ;; (dolist (face '((org-level-1 . 1.05)
  ;;              (org-level-2 . 1.05)
  ;;              (org-level-3 . 1.05)
  ;;              (org-level-4 . 1.05)))
  ;;   (set-face-attribute (car face) nil :font "Terminus" :weight 'medium :height (cdr face)))
  (set-face-attribute 'org-block nil :background "#101010"))

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

;;;; Templates
(use-package org-tempo
  :ensure nil
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh"  . "src sh"))
  (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("vim"  . "src vim")))

(use-package org-transclusion
  :after org
  :bind ("C-c t" . org-transclusion-add))

(use-package org-protocol
  :ensure nil
  :after org)
