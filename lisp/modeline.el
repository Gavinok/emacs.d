;; determine the focused window
(defvar cogent-line-selected-window (frame-selected-window))
(defun cogent-line-set-selected-window (&rest _args)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq cogent-line-selected-window (frame-selected-window))
    (force-mode-line-update)))
(defun cogent-line-unset-selected-window ()
  (setq cogent-line-selected-window nil)
  (force-mode-line-update))
(defun cogent-line-selected-window-active-p ()
  (eq cogent-line-selected-window (selected-window)))

(add-hook 'window-configuration-change-hook #'cogent-line-set-selected-window)
(add-hook 'window-selection-change-functions #'cogent-line-set-selected-window)

(setq my/mode-line-right-side '(:eval (when (mode-line-window-selected-p)
                                        (list
                                         '(vc-mode vc-mode)
                                         " "
                                         mode-line-misc-info
                                         )
                                        )))
(setq my/mode-line-left-side '(" "
                               ;; indicate if the buffer has been modified
                               (:eval (propertize (if (and (not buffer-read-only) (buffer-modified-p))
                                                      "● " "  " )
                                                  'face 'error))

                               ;; Buffer name (no longer than 1/3 of the screen)
                               ;; mode-line-remote
                               mode-line-buffer-identification
                               mode-line-process
                               (:eval (when (mode-line-window-selected-p)" %l:%c"))
                               ))

(setq-default mode-line-format
              `(
                ,@my/mode-line-left-side
                ;; I still have yet to come up with a better option
                mode-line-format-right-align
                ,my/mode-line-right-side))


(use-package time
  :defer 10
  :config
  (defface my/display-time
    '((((type x w32 mac))
       ;; #060525 is the background colour of my default face.
       (:foreground "#060525" :inherit bold))
      (((type tty))
       (:foreground "blue")))
    "Face used to display the time in the mode line.")
  (setq display-time-string-forms
        '((propertize (concat " " 12-hours ":" minutes " " am-pm " ")
                      'face 'my/display-time)))
  (set-face-attribute 'my/display-time nil :foreground "#fff" :background "#333"
                      ;; :box '(:line-width 1 :color "#323")
                      )
  (display-time-mode t))

(set-face-attribute 'appt-notification nil :foreground "#fff" :background "#333")

(set-face-attribute  'org-mode-line-clock nil :foreground "#fff" :background "#333"
                     ;; :box '(:line-width 1 :color "#323")
                     :inherit 'bold)
