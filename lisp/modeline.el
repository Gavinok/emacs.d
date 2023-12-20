(defvar ml-text-scale-factor 1.0
  "Scale of mode-line font size to default font size, as a float.
This is needed to make sure that text is properly aligned.")

(defun ml-fill-to-center (reserve face)
  "Return empty space to the center, leaving RESERVE space on the right."
  (when ml-text-scale-factor
    (setq reserve (* ml-text-scale-factor reserve)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin))
                                             ,reserve
                                             (.5 . left-margin))))
              'face face))

(defun ml-fill-to-right (reserve face)
  "Return empty space, leaving RESERVE space on the right."
  (when ml-text-scale-factor
    (setq reserve (* ml-text-scale-factor reserve)))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 2))) ; Powerline uses 3 here, but my scrollbars are narrower.
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                             ,reserve)))
              'face face))

(defun ml-render-2-part (left right &optional fill-face)
  (concat left
          (ml-fill-to-right (string-width (format-mode-line right)) fill-face)
          right))


;; determin the focused window
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
                                         (propertize (abbreviate-file-name (project-root (project-current))) 'face 'shadow )
                                         '(vc-mode vc-mode) " "
                                         mode-line-misc-info))))
(setq my/mode-line-left-side '(" "
                               ;; indicate if the buffer has been modified
                               (:eval (propertize
                                       (if (and (not buffer-read-only) (buffer-modified-p))
                                           "● " "  " )
                                       'face 'error))

                               ;; Buffer name (no longer than 1/3 of the screen)
                               ;; mode-line-remote
                               mode-line-buffer-identification
                               " "
                               (:eval (when (mode-line-window-selected-p)" %l:%c"))
                               (:eval (when (mode-line-window-selected-p)
                                        (propertize
                                         (concat " %p%"
                                                 "  "
                                                 "「 %m 」")
                                         'face (if (cogent-line-selected-window-active-p)
                                                   'shadow
                                                 'mode-line-inactive))))))

(setq my/mode-line-padding
      '(:eval
        (propertize " " 'display
	                  `((space :align-to
			                       (- (+ right right-fringe right-margin)
			                          ,(string-width
                                  (format-mode-line my/mode-line-right-side))))))))

(setq-default mode-line-format
              `(
                ,@my/mode-line-left-side
                ;; I still have yet to come up with a better option
                ,my/mode-line-padding
                ,my/mode-line-right-side))


;; (defun vc-branch ()
;;   (let ((backend (vc-backend buffer-file-name)))
;;     (substring vc-mode (+ (if (eq backend 'hg) 2 3) 2))))
;; (vc-branch)

(set-face-attribute 'mode-line-inactive nil :foreground "#555" :background "#222"
                    :box '(:line-width 2 :color "#222"))
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
                      :box '(:line-width 1 :color "#323"))
  (display-time-mode t))
(set-face-attribute  'org-mode-line-clock nil :foreground "#fff" :background "#333"
                     :box '(:line-width 1 :color "#323") :inherit 'bold)

