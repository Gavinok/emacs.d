(use-package evil
  :ensure t
  :commands (evil-mode)
  :bind (:map evil-normal-state-map
              ;; vim vinigar style
              ("-"  . (lambda () (interactive)
                        (dired ".")))
              ("C-s" . consult-line)
              ;; Better lisp bindings
              ("(" . evil-previous-open-paren)
              (")" . evil-next-close-paren)
              ("<leader>/" . evil-ex-nohighlight)
              ("C-n" . evil-next-line)
              ("C-p" . evil-previous-line)
              :map evil-operator-state-map
              ("(" . evil-previous-open-paren)
              (")" . evil-previous-close-paren))
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)

  ;; no vim insert bindings
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-split-window-below t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-split-window-right t)
  :config
  (evil-set-leader 'normal " "))

;; Enable Commentary
(use-package evil-commentary
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)))

;; Enable Surround
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))
