;;; Extras
(use-package quelpa-use-package)
;; Don't forget to run M-x eaf-install-dependencies
(use-package eaf
  :unless my/is-termux
  :bind (("C-c w" . my/bm)
         ("s-w" . my/bm))
  ;; :demand t
  ;; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/"
  :init
  (use-package epc      :defer t :ensure t)
  (use-package ctable   :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s        :defer t :ensure t)
  ;; (setq browse-url-browser-function 'eaf-open-browser)
  :config
  (defun slurp (f)
    (with-temp-buffer
      (insert-file-contents f)
      (buffer-substring-no-properties
       (point-min)
       (point-max))))
  (defun my/bm ()
    (interactive)
    (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/browser/")
    (require 'eaf-browser)

    (let ((selected (completing-read
                     "Select URL: " (split-string
                                     (slurp "~/.config/bookmarks") "\n" t))))
      (let ((url (car (split-string
                       selected
                       " " t))))
        (if (string-match-p "\\http.*\\'" url)
            ;; Open selected url
            (eaf-open-browser url)
          ;; Search entered text
          (eaf-search-it selected)))))
  (setq eaf-browser-continue-where-left-off t)
  (setq eaf-browser-dnefault-search-engine "duckduckgo")
  (setq eaf-browser-enable-adblocker "true")
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (dolist (keys '("i" "h" "j" "k" "l"
  ;;              "d" "f" "x" ","
  ;;              "-" "." "0" "1"
  ;;              "2" "=" "B" "F"
  ;;              "G" "H" "I" "J"
  ;;              "K" "L" "P" "T"
  ;;              "Y" "c" "d" "e"
  ;;              "f" "g" "m" "n"
  ;;              "o" "p" "r" "t"
  ;;              "u" "v" "x" "y"
  ;;              (eaf-bind-key nil key eaf-browser-keybinding))))
  )
