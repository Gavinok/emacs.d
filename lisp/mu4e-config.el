;;; EMAIL
;;;; Org In Emails
(use-package org-mime
  :ensure t
  :after mu4e
  :bind (:map org-mode-map
              ("C-c M-o" . org-mime-org-subtree-htmlize))
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                                   :with-author nil
                                                   :with-toc nil)))
(use-package mu4e
  :unless my/is-termux
  :ensure nil
  :commands (mu4e-user-agent)
  :bind (("C-x M" . mu4e)
         ("C-x m" . mu4e-compose-new)
         ([remap mu4e-headers-jump-to-maildir] . my/jump-to-maildir)
         :map message-mode-map
         ("C-M-i" . completion-at-point))
  :demand t
  ;; :commands (mu4e mu4e-user-agent mu4e-compose-new org-mime-org-subtree-htmlize)
  :init
  (setopt mu4e-modeline-mode nil)
  ;; Show full email address
  (setopt mu4e-completing-read-function 'completing-read
          ;; mu4e-view-show-addresses 't
          mu4e-maildir "~/.local/share/mail"
          ;; where to put attachemnts
          mu4e-attachment-dir  "~/Downloads"
          ;; use mu4e for e-mail in emacs
          mail-user-agent #'mu4e-user-agent
          message-mail-user-agent t)
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

;;;; mbsync and msmtp setup
  (setq mu4e-get-mail-command "mailsync"
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask
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

  (set-face-attribute 'mu4e-thread-fold-face nil :inherit 'default :foreground "#fff")

  ;; Fold using - instead of •
  (defun mu4e-thread-fold-info (count unread)
    "Text to be displayed for a folded thread.
There are COUNT hidden and UNREAD messages overall."
    (let ((size  (+ 2 (apply #'+ (mapcar (lambda (item) (or (cdr item) 0))
                                         mu4e-headers-fields))))
          (msg (concat (format"[%d hidden messages%s]\n" count
                              (if (> unread 0)
                                  (format ", %d unread" unread)
                                "")))))
      (propertize (concat "  " (make-string size ?-) " " msg))))

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
         (make-mu4e-context
          :name "Work"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/quartech" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address                . "Gavin.Freeborn@quartech.com")
                  (user-full-name                   . "Gavin Jaeger-Freeborn")
                  (mu4e-drafts-folder               . "/quartech/Drafts")
                  (mu4e-sent-folder                 . "/quartech/Sent")
                  (mu4e-refile-folder               . "/quartech/INBOX")
                  (mu4e-trash-folder                . "/quartech/Trash")))))
;;;; Contacts
  (setq mail-personal-alias-file  "~/.config/mutt/aliases")
  (setq mu4e-org-contacts-file  (concat org-directory "/contacts.org"))
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
;;;; Bookmarks
  (setopt mu4e-bookmarks '((:name "Main INBOX"
                                  :query "maildir:/personal/INBOX or maildir:/quartech/INBOX" :key 115)
                           (:name "To Handle"
                                  :query "((flag:flagged AND (NOT flag:replied)) OR (NOT flag:seen)) AND (NOT flag:trashed) AND (NOT maildir:/personal/[Gmail].Spam) AND date:1y..now)" :key 116)
                           (:name "Today's messages"
                                  :query "date:today..now" :key 118)
                           (:name "Last 7 days"
                                  :query "date:7d..now" :hide-unread t :key 119)
                           (:name "Messages with images"
                                  :query "mime:image/*" :key 112))) ; email client depends on mu command

;;;; Headers View
  (setq mu4e-use-fancy-chars t)
  (setopt
   mu4e-headers-draft-mark     '("D" . "✍")
   mu4e-headers-flagged-mark   '("F" . "✡")
   mu4e-headers-new-mark       '("N" . "•")
   mu4e-headers-calendar-mark  '("c" . "📅"))
  (setq mu4e-search-sort-field :date)
  (setq mu4e-search-threads t)
  (setopt mu4e-headers-visible-flags '(new flagged))
  (setq mu4e-headers-fields '((:flags         .    6)
                              (:from-or-to    .   22)
                              (:subject       .   70)
                              (:human-date    .   nil))))
