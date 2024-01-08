;;; EMAIL
(use-package org-msg
  :ensure t
  :after mu4e
  :bind (:map org-msg-edit-mode-map
              ("M-g M-g" . my/org-msg-goto-body))
  :config
  (defun my/org-msg-goto-body (&optional end)
    "Go to either the beginning or the end of the body.
END can be the symbol top, bottom, or nil to toggle."
    (interactive)
    (let ((initial-pos (point)))
      (org-msg-goto-body)
      (when (or (eq end 'top)
                (and (or (eq initial-pos (point)) ; Already at bottom
                         (<= initial-pos ; Above message body
                             (save-excursion
                               (message-goto-body)
                               (point))))
                     (not (eq end 'bottom))))
        (message-goto-body)
        (search-forward ",")
        (backward-char))))
  (setopt org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	  org-msg-startup "hidestars indent inlineimages"
	  org-msg-greeting-fmt "\nHello%s,\n\n"
	  org-msg-greeting-name-limit 3
	  org-msg-default-alternatives '((new		. (text html))
				         (reply-to-html	. (text html))
				         (reply-to-text	. (text)))
	  org-msg-convert-citation t
          org-msg-signature (concat "\n\n#+begin_signature\nSincerely,\n@@html:<b>@@" user-full-name "@@html:</b>@@\n#+end_signature"))
  (org-msg-mode)
  )
(use-package mu4e
  :unless my/is-termux
  :ensure nil
  :commands (mu4e-user-agent)
  :bind (("C-x M" . mu4e)
         ("C-x m" . mu4e-compose-new)
         ([remap mu4e-headers-jump-to-maildir] . my/jump-to-maildir)
         )
  :demand t
  ;; :commands (mu4e mu4e-user-agent mu4e-compose-new org-mime-org-subtree-htmlize)
  :init
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

;;;; Org In Emails
  (use-package org-mime
    :ensure t
    :after mu4e
    :bind (:map org-mode-map
                ("C-c m" . org-mime-org-subtree-htmlize))
    :config
    (setq org-mime-export-options '(:section-numbers nil
                                                     :with-author nil
                                                     :with-toc nil)))
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
  (setq mu4e-bookmarks '((:name "Main INBOX"
                                :query "maildir:/personal/INBOX or maildir:/quartech/INBOX" :key 115)
                         (:name "To Handle"
                                :query "((flag:flagged AND (NOT flag:replied)) OR (NOT flag:seen))" :key 116)
                         (:name "Today's messages"
                                :query "date:today..now" :key 118)
                         (:name "Last 7 days"
                                :query "date:7d..now" :hide-unread t :key 119)
                         (:name "Messages with images"
                                :query "mime:image/*" :key 112))) ; email client depends on mu command

;;;; Headers View
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-header-sort-field :date)
  (setq mu4e-search-threads t)
  (setq mu4e-headers-fields '((:flags         .    6)
                              (:from-or-to    .   22)
                              (:subject       .   70)
                              (:human-date    .   nil))))
