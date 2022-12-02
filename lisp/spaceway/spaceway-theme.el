;;; spaceway-theme.el --- Spaceway theme for GNU Emacs 24 (deftheme)

;; Author: Mark Tran <mark.tran@gmail.com>
;; URL: http://github.com/marktran/color-theme-spaceway
;; Package-Version: 20180214.1624
;; Package-Commit: bf724ce7806a738d2043544061e5f9bbfc56e674
;; Version: 1.2.9

;; Inspired by jellybeans: http://www.vim.org/scripts/script.php?script_id=2555.
(deftheme spaceway "The spaceway color theme")

(let ((class '((class color) (min-colors 89)))
      (spaceway-fg "#CDCDCD")
      (spaceway-bg (if (display-graphic-p) ;; "#161A1F"
                     "#000000"
                     nil))
      (spaceway-blue-0 "#7D8FA3")
      (spaceway-region "#8fafd7")
      (spaceway-green-0 "#a1bf78")
      (spaceway-green-1 "#a1bf78")
      (spaceway-green-2 "#85A7A5")
      (spaceway-green-3 "#75b5aa")
      (spaceway-grey-0 "#888888")
      (spaceway-grey-1 "#7f7f7f")
      (spaceway-grey-2 "#151515")
      (spaceway-grey-3 "#1c1c1c")
      (spaceway-grey-4 "#363636")
      (spaceway-grey-5 "#444444")
      (spaceway-orange-0 "#E5C078")
      (spaceway-orange-1 "#f4bf75")
      (spaceway-purple-0 "#8197bf")
      (spaceway-purple-1 "#474e90")
      (spaceway-purple-2 "#9F7AA5")
      (spaceway-purple-3 "#aa759f")
      (spaceway-red-0 "#CC6666")
      (spaceway-red-1 "#af5f5f")
      (spaceway-red-2 "#d75f5f")
      (spaceway-red-3 "#ff73fd")
      (spaceway-yellow-0 "#E5C078")
      (spaceway-yellow-1 "#FFAF00")
      (spaceway-hl-line "#14171B" ;; "#101010"
                        ))

  (custom-theme-set-faces
   'spaceway
   `(default ((,class (:foreground ,spaceway-fg :background ,spaceway-bg))))
   `(alchemist-test--failed-face ((,class (:foreground ,spaceway-red-0))))
   `(alchemist-test--success-face ((,class (:foreground ,spaceway-green-0))))
   `(avy-lead-face ((,class (:foreground ,spaceway-fg :background ,spaceway-red-0))))
   `(avy-lead-face-0 ((,class (:foreground ,spaceway-fg :background ,spaceway-green-0))))
   `(bm-face ((,class (:background ,spaceway-grey-4))))
   `(mu4e-header-highlight-face ((,class (:inherit hl-line :underline nil  :weight bold))))
   `(mu4e-flagged-face ((,class (:foreground ,spaceway-red-0 :background ,spaceway-bg))))
   `(mu4e-unread-face ((,class (:foreground ,spaceway-red-0 :background ,spaceway-bg))))
   `(company-preview-common ((,class (:foreground nil :background ,spaceway-purple-1))))
   `(company-scrollbar-bg ((,class (:background ,spaceway-grey-2))))
   `(company-scrollbar-fg ((,class (:background ,spaceway-grey-0))))
   `(company-tooltip ((,class (:foreground ,spaceway-fg :background ,spaceway-grey-2))))
   `(company-tooltip-common ((,class (:foreground ,spaceway-grey-1 :background ,spaceway-grey-2))))
   `(company-tooltip-common-selection ((,class (:foreground ,spaceway-grey-1 :background ,spaceway-purple-1))))
   `(company-tooltip-selection ((,class (:background ,spaceway-purple-1))))
   `(compilation-error ((,class (:foreground ,spaceway-red-0))))
   `(compilation-info ((,class (:foreground ,spaceway-yellow-0))))
   `(compilation-line-number ((,class (:foreground ,spaceway-grey-0))))
   `(compilation-mode-line-exit ((,class (:foreground ,spaceway-green-0))))
   `(compilation-mode-line-fail ((,class (:foreground ,spaceway-red-0))))
   `(compilation-mode-line-run ((,class (:foreground ,spaceway-yellow-0))))
   `(lsp-ui-doc-background ((,class (:foreground ,spaceway-fg :background ,spaceway-bg))))
   `(diredp-date-time ((,class (:foreground ,spaceway-fg))))
   `(dired-directory ((,class (:foreground ,spaceway-blue-0))))
   `(diredp-deletion ((,class (:foreground ,spaceway-red-0 :background ,spaceway-bg))))
   `(diredp-dir-heading ((,class (:foreground ,spaceway-yellow-0 :background ,spaceway-bg))))
   `(diredp-dir-name ((,class (:foreground ,spaceway-green-2 :background ,spaceway-bg))))
   `(diredp-dir-priv ((,class (:foreground ,spaceway-green-2 :background ,spaceway-bg))))
   `(diredp-exec-priv ((,class (:foreground ,spaceway-fg :background ,spaceway-bg))))
   `(diredp-file-name ((,class (:foreground ,spaceway-fg))))
   `(diredp-file-suffix ((,class (:foreground ,spaceway-fg))))
   `(diredp-link-priv ((,class (:foreground ,spaceway-fg))))
   `(diredp-number ((,class (:foreground ,spaceway-fg))))
   `(diredp-no-priv ((,class (:foreground ,spaceway-fg :background ,spaceway-bg))))
   `(diredp-rare-priv ((,class (:foreground ,spaceway-red-0 :background ,spaceway-bg))))
   `(diredp-read-priv ((,class (:foreground ,spaceway-fg :background ,spaceway-bg))))
   `(diredp-symlink ((,class (:foreground ,spaceway-red-3))))
   `(diredp-write-priv ((,class (:foreground ,spaceway-fg :background ,spaceway-bg))))
   `(ediff-odd-diff-A ((,class (:foreground ,spaceway-red-0 :background ,spaceway-grey-2))))
   `(ediff-odd-diff-B ((,class (:foreground ,spaceway-green-0 :background ,spaceway-grey-2))))
   `(emmet-preview-output ((,class (:background ,spaceway-purple-1))))
   `(elixir-atom-face ((,class (:foreground ,spaceway-blue-0))))
   `(elixir-attribute-face ((,class (:foreground ,spaceway-red-0))))
   `(enh-ruby-op-face ((,class (:foreground ,spaceway-purple-0))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,spaceway-purple-3))))
   `(erc-notice-face ((,class (:foreground ,spaceway-yellow-0))))
   `(erc-prompt-face ((,class (:foreground ,spaceway-fg))))
   `(erc-timestamp-face ((,class (:foreground ,spaceway-purple-0))))
   `(eshell-prompt ((,class (:foreground ,spaceway-red-0))))
   `(eshell-ls-directory ((,class (:weight normal :foreground ,spaceway-green-2))))
   `(eshell-ls-executable ((,class (:weight normal :foreground ,spaceway-red-0))))
   `(eshell-ls-product ((,class (:foreground ,spaceway-fg))))
   `(eshell-ls-symlink ((,class (:weight normal :foreground ,spaceway-purple-2))))
   `(flycheck-error ((,class (:background ,spaceway-grey-4))))
   `(flycheck-error-list-column-number ((,class (:foreground ,spaceway-fg))))
   `(flycheck-error-list-line-number ((,class (:foreground ,spaceway-fg))))
   `(flycheck-error-list-id ((,class (:foreground ,spaceway-fg))))
   `(flycheck-error-list-info ((,class (:foreground ,spaceway-yellow-0))))
   `(flycheck-info ((,class (:background ,spaceway-grey-4))))
   `(flycheck-warning ((,class (:background ,spaceway-grey-4))))
   `(font-lock-builtin-face ((,class (:foreground ,spaceway-blue-0))))
   `(font-lock-comment-face ((,class (:slant italic :foreground ,spaceway-grey-0))))
   `(font-lock-constant-face ((,class (:foreground ,spaceway-orange-1))))
   `(font-lock-doc-face ((,class (:foreground ,spaceway-green-0))))
   `(font-lock-function-name-face ((,class (:foreground ,spaceway-fg))))
   `(font-lock-keyword-face ((,class (:foreground ,spaceway-purple-0))))
   `(font-lock-preprocessor-face ((,class (:foreground ,spaceway-green-2))))
   `(font-lock-string-face ((,class (:foreground ,spaceway-green-0))))
   `(font-lock-type-face ((,class (:foreground ,spaceway-orange-0))))
   `(font-lock-variable-name-face ((,class (:foreground ,spaceway-fg))))
   `(font-lock-warning-face ((,class (:foreground ,spaceway-red-1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,spaceway-yellow-0 :bold t))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,spaceway-red-0 :bold t))))
   `(fringe ((,class (:foreground ,spaceway-fg :background ,spaceway-bg))))
   `(git-commit-comment-file ((,class (:foreground ,spaceway-fg))))
   `(git-commit-comment-heading ((,class (:foreground ,spaceway-yellow-0))))
   `(git-commit-summary ((,class (:foreground ,spaceway-fg))))
   `(parenthesis ((,class (:foreground ,spaceway-grey-5))))
   `(keycast-key ((,class (:weight normal ;; :box '(:line-width 10 :color "#000")
                                   :foreground ,spaceway-fg ;; "000"
                                   :background ,spaceway-hl-line ;; "#222"
                                   ))))
   `(helm-buffer-file ((,class (:foreground ,spaceway-fg))))
   `(helm-buffer-process ((,class (:foreground ,spaceway-yellow-0))))
   `(helm-buffer-size ((,class (:foreground ,spaceway-fg))))
   `(helm-candidate-number ((,class (:foreground ,spaceway-fg :background ,spaceway-bg))))
   `(helm-ff-directory ((,class (:foreground ,spaceway-green-2))))
   `(helm-ff-dotted-directory ((,class (:foreground ,spaceway-green-2 :background ,spaceway-bg))))
   `(helm-ff-dotted-symlink-directory ((,class (:foreground ,spaceway-green-2 :background ,spaceway-bg))))
   `(helm-ff-executable ((,class (:foreground ,spaceway-red-0))))
   `(helm-ff-file ((,class (:foreground ,spaceway-fg))))
   `(helm-ff-symlink ((,class (:foreground ,spaceway-purple-2))))
   `(helm-grep-lineno ((,class (:foreground ,spaceway-fg))))
   `(helm-match ((,class (:foreground ,spaceway-red-1 :background ,spaceway-bg))))
   `(helm-moccur-buffer ((,class (:foreground ,spaceway-yellow-0))))
   `(helm-selection ((,class (:background ,spaceway-purple-1))))
   `(helm-source-header ((,class (:foreground ,spaceway-yellow-0 :background ,spaceway-grey-3))))
   `(helm-swoop-target-line-face ((,class (:foreground ,spaceway-fg :background ,spaceway-grey-4))))
   `(helm-swoop-target-word-face ((,class (:foreground ,spaceway-red-1))))
   `(highlight ((,class (:background ,spaceway-grey-4))))
   `(highlight-indentation-face ((,class (:background ,spaceway-grey-4))))
   `(highlight-indentation-current-column-face ((,class (:background ,spaceway-grey-4))))
   `(hl-line ((,class (:background ,spaceway-hl-line))))
   `(iedit-occurrence ((,class (:foreground ,spaceway-red-1))))
   `(isearch ((,class (:foreground ,spaceway-fg :background ,spaceway-red-1))))
   `(isearch-fail ((,class (:background ,spaceway-red-1))))
   `(ido-first-match ((,class (:foreground ,spaceway-yellow-0))))
   `(ido-only-match ((,class (:foreground ,spaceway-green-0))))
   `(ido-subdir ((,class (:foreground ,spaceway-fg))))
   `(ido-virtual ((,class (:foreground ,spaceway-purple-0))))
   `(ivy-current-match ((,class (:background ,spaceway-purple-1))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,spaceway-orange-0))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,spaceway-orange-0))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,spaceway-orange-0))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,spaceway-orange-0))))
   `(js2-external-variable ((,class (:foreground ,spaceway-yellow-0))))
   `(js2-function-param ((,class (:foreground ,spaceway-fg))))
   `(lazy-highlight ((,class (:foreground ,spaceway-red-1 :background nil))))
   `(linum ((,class (:slant italic :foreground ,spaceway-grey-4))))
   `(magit-blame-heading ((,class (:foreground ,spaceway-grey-1 :background ,spaceway-grey-2))))
   `(magit-branch-local ((,class (:foreground ,spaceway-green-2))))
   `(magit-branch-remote ((,class (:foreground ,spaceway-green-2))))
   `(magit-section-heading ((,class (:foreground ,spaceway-yellow-0 :background ,spaceway-grey-2))))
   `(magit-diff-added ((,class (:foreground ,spaceway-green-0))))
   `(magit-diff-added-highlight ((,class (:foreground ,spaceway-green-0 :inherit (magit-section-highlight)))))
   `(magit-diff-context ((,class (:foreground ,spaceway-fg))))
   `(magit-diff-context-highlight ((,class (:foreground ,spaceway-fg :inherit (magit-section-highlight)))))
   `(magit-diff-file-heading ((,class (:weight normal :foreground ,spaceway-fg :background ,spaceway-bg))))
   `(magit-diff-file-heading-highlight ((,class (:weight normal :foreground ,spaceway-fg :background ,spaceway-grey-2))))
   `(magit-diff-hunk-heading ((,class (:foreground ,spaceway-yellow-0 :background ,spaceway-grey-2))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,spaceway-yellow-0 :background ,spaceway-grey-3))))
   `(magit-diff-lines-heading ((,class (:foreground ,spaceway-fg :background ,spaceway-purple-1))))
   `(magit-diff-removed ((,class (:foreground ,spaceway-red-0))))
   `(magit-diff-removed-highlight ((,class (:foreground ,spaceway-red-0 :inherit (magit-section-highlight)))))
   `(magit-diffstat-added ((,class (:foreground ,spaceway-green-0))))
   `(magit-diffstat-removed ((,class (:foreground ,spaceway-red-0))))
   `(magit-hash ((,class (:foreground ,spaceway-red-0))))
   `(magit-log-author ((,class (:foreground ,spaceway-yellow-0))))
   `(magit-mode-line-process ((,class (:foreground ,spaceway-yellow-0))))
   `(magit-mode-line-process-error ((,class (:foreground ,spaceway-red-0))))
   `(magit-popup-argument ((,class (:foreground ,spaceway-red-0))))
   `(magit-popup-disabled-argument ((,class (:foreground ,spaceway-grey-0))))
   `(magit-section-highlight ((,class (:background ,spaceway-grey-2))))

   ;; vterm and friends
   `(term-color-black ((,class (:foreground ,spaceway-grey-3 :background ,spaceway-grey-3))))
   `(term-color-bright-black ((,class (:foreground ,spaceway-grey-2 :background ,spaceway-grey-2))))
   `(term-color-red ((,class (:foreground ,spaceway-red-1 :background ,spaceway-red-1))))
   `(term-color-bright-red ((,class (:foreground ,spaceway-red-2 :background ,spaceway-red-2))))
   `(term-color-green ((,class (:foreground ,spaceway-green-2 :background ,spaceway-green-2))))
   `(term-color-bright-green ((,class (:foreground ,spaceway-green-3 :background ,spaceway-green-3))))
   `(term-color-yellow ((,class (:foreground ,spaceway-orange-0 :background ,spaceway-orange-0))))
   `(term-color-bright-yellow ((,class (:foreground ,spaceway-yellow-1 :background ,spaceway-yellow-1))))
   `(term-color-blue ((,class (:foreground ,spaceway-blue-0 :background ,spaceway-blue-0))))
   `(term-color-bright-blue ((,class (:foreground ,spaceway-region :background ,spaceway-region))))
   `(term-color-magenta ((,class (:foreground ,spaceway-red-2 :background ,spaceway-red-2))))
   `(term-color-cyan ((,class (:foreground ,spaceway-purple-0 :background ,spaceway-purple-0))))
   `(term-color-white ((,class (:foreground ,spaceway-fg :background ,spaceway-fg))))
   ;; `(term-color-black ((,class (:foreground ,spaceway-black-2 :background ,spaceway-black-2))))
   ;; `(term-color-black ((,class (:foreground ,spaceway-black-2 :background ,spaceway-black-2))))
   ;; `(term-color-black ((,class (:foreground ,spaceway-black-2 :background ,spaceway-black-2))))
       
   `(match ((,class (:background ,spaceway-red-1))))
   `(minibuffer-prompt ((,class (:foreground ,spaceway-fg))))
   `(mmm-default-submode-face ((,class (:background ,spaceway-bg))))
   `(header-line ((,class (:foreground ,spaceway-fg :background nil))))
   `(tab-bar ((,class (:foreground ;; "#EEDC82"
                       ,spaceway-fg
                       :background "#333" ;; nil
                       ))))
   `(tab-bar-tab-inactive ((,class (:foreground ;; "#EEDC82"
                                    ,spaceway-grey-1
                                    :background "#333" ;; nil
                                    ))))
   `(mode-line ((,class (:foreground ;; "#EEDC82"
                         ,spaceway-fg
                         :background "#333" ;; nil
                         (:line-width 1 :style released-button) 1
                         ))))
   `(mode-line-inactive ((,class (:foreground ,"#555" ;; spaceway-grey-4
                                              :background "#222" ;; nil
                                              ))))
   `(org-timeline-block ((,class (:foreground ,spaceway-bg :background ,spaceway-red-0))))
   `(org-timeline-elapsed ((,class (:foreground ,spaceway-fg :background ,spaceway-grey-2))))
   `(org-ellipsis ((,class (:foreground ,spaceway-grey-1 :background nil :underline nil))))
   `(org-checkbox ((,class (:foreground ,spaceway-green-2))))
   `(org-date ((,class (:foreground ,spaceway-grey-0))))
   `(org-document-info-keyword ((,class (:foreground ,spaceway-yellow-0))))
   `(org-document-title ((,class (:foreground ,spaceway-yellow-0))))
   `(org-verbatim ((,class (:foreground ,spaceway-blue-0))))
   `(org-code ((,class (:foreground ,spaceway-purple-0))))
   `(org-done ((,class (:foreground ,spaceway-green-3))))
   `(org-level-1 ((,class (:foreground ,spaceway-red-2 ;; :variable-pitch
                                       ))))
   `(org-level-2 ((,class (:foreground ,spaceway-yellow-0))))
   `(org-level-3 ((,class (:foreground ,spaceway-green-2))))
   `(org-level-4 ((,class (:foreground ,spaceway-orange-0))))
   `(org-level-5 ((,class (:foreground ,spaceway-purple-3))))
   `(org-level-6 ((,class (:foreground ,spaceway-red-0))))
   `(org-level-7 ((,class (:foreground ,spaceway-blue-0))))
   `(org-level-8 ((,class (:foreground ,spaceway-purple-3))))
   `(org-link ((,class (:foreground ,spaceway-blue-0))))
   `(org-meta-line ((,class (:foreground ,spaceway-grey-0))))
   `(org-special-keyword ((,class (:foreground ,spaceway-purple-0))))
   `(org-upcoming-deadline ((,class (:background ,spaceway-red-0))))
   `(org-block ((,class (:background
                         "#14171B"
                         ;; "#010101"
                         ))))
   `(org-todo ((,class (:foreground ,spaceway-red-1))))
   `(persp-selected-face ((,class (:foreground ,spaceway-orange-0))))
   `(region ((,class (:foreground ,spaceway-bg
                                  :background ,spaceway-region))))
   `(shm-current-face ((,class (:background ,spaceway-grey-4))))
   `(shm-quarantine-face ((,class (:background ,spaceway-red-1))))
   `(smerge-markers ((,class (:foreground ,spaceway-yellow-0 :background ,spaceway-grey-2))))
   `(smerge-mine ((,class (:foreground ,spaceway-fg :background ,spaceway-purple-3))))
   `(smerge-other ((,class (:foreground ,spaceway-fg :background ,spaceway-green-1))))
   `(smerge-refined-change ((,class (:foreground ,spaceway-green-0))))
   `(sp-pair-overlay-face ((,class (:background ,spaceway-grey-4))))
   `(sp-show-pair-match-face ((,class (:background ,spaceway-grey-5))))
   `(swiper-match-face-1 ((,class (:foreground ,spaceway-red-1))))
   `(swiper-match-face-2 ((,class (:foreground ,spaceway-red-1))))
   `(swiper-match-face-3 ((,class (:foreground ,spaceway-red-1))))
   `(swiper-match-face-4 ((,class (:foreground ,spaceway-red-1))))
   `(trailing-whitespace ((,class (:background ,spaceway-red-1))))
   `(warning ((,class (:foreground ,spaceway-orange-0))))
   `(web-mode-builtin-face ((,class (:foreground ,spaceway-blue-0))))
   `(web-mode-constant-face ((,class (:foreground ,spaceway-purple-0))))
   `(web-mode-css-at-rule-face ((,class (:foreground ,spaceway-blue-0))))
   `(web-mode-css-function-face ((,class (:foreground ,spaceway-orange-0))))
   `(web-mode-css-property-name-face ((,class (:foreground ,spaceway-green-3))))
   `(web-mode-css-selector-face ((,class (:foreground ,spaceway-yellow-0))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,spaceway-purple-0))))
   `(web-mode-html-tag-face ((,class (:foreground ,spaceway-fg))))
   `(web-mode-symbol-face ((,class (:foreground ,spaceway-green-1))))
   `(which-key-group-description-face ((,class (:foreground ,spaceway-grey-0))))
   `(which-key-key-face ((,class (:foreground ,spaceway-green-0))))
   `(whitespace-trailing ((,class (:background ,spaceway-red-1))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'spaceway)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; spaceway-theme.el ends here
