;; -*- lexical-binding: t -*-
(use-package affe
  :bind (("M-s M-f" . affe-find)
	 ("M-s f"   . affe-find)
	 ("M-s M-g" . affe-grep)
	 ("M-s g"   . affe-grep))
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-find affe-grep :preview-key (kbd "M-.")))
