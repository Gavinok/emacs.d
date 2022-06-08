(use-package exwm
  :unless my/is-termux
  :ensure t
  :init
  (setq exwm-workspace-number 4)
  :config
  (setq exwm-manage-force-tiling t)
 ;;;; Hooks
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

    ;; (add-hook 'exwm-manage-finish-hook
    ;;         (lambda ()
    ;;           (when (and exwm-class-name
    ;;                   (or (string= exwm-class-name "qutebrowser")
    ;;                       (string= exwm-class-name "libreoffice-writer")
    ;;                       (string= exwm-class-name "libreoffice-calc")
    ;;                       (string= exwm-class-name "Google-chrome")
    ;;                       (string= exwm-class-name "Brave-browser")))
    ;;          (exwm-input-set-local-simulation-keys nil))))

    (add-hook 'exwm-update-title-hook
            (lambda ()
              (pcase exwm-class-name
                ("qutebrowser" (exwm-workspace-rename-buffer (format "%s" exwm-title)))
                ("libreoffice-writer" (exwm-workspace-rename-buffer (format "Writer: %s" exwm-title)))
                ("libreoffice-calc" (exwm-workspace-rename-buffer (format "Calc: %s" exwm-title)))
                ("St" (exwm-workspace-rename-buffer (format "%s" exwm-title))))))
    ;; Hide the modeline on all X windows
    (add-hook 'exwm-floating-setup-hook
              (lambda ()
                (exwm-layout-hide-mode-line)))
;;;; Funcs
  (defun my/switch-to-scratch-and-back ()
    "Toggle between *scratch* buffer and the current buffer.
     If the *scratch* buffer does not exist, create it."
    (interactive)
    (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
      (if (equal (current-buffer) scratch-buffer-name)
          (switch-to-buffer (other-buffer))
        (switch-to-buffer scratch-buffer-name (lisp-interaction-mode)))))

  ;;Just disallow killing of scratch buffer
  (defun my/unkillable-scratch-buffer ()
    (if (equal (buffer-name (current-buffer)) "*scratch*")
        (progn
          (delete-region (point-min) (point-max))
          nil)
      t))

  (add-hook 'kill-buffer-query-functions 'my/unkillable-scratch-buffer)
;;;; Global Key Bindings
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (when (and exwm-class-name
                         (or (string= exwm-class-name "Google-chrome")
                             (string= exwm-class-name "discord")))
                (exwm-input-set-local-simulation-keys `(([?\C-b] . [left])
							([?\C-f] . [right])
                                                        ([?\M-b] . ,(kbd "C-<left>"))
                                                        ([?\M-f] . ,(kbd "C-<right>"))
                                                        ([?\M-d] . ,(kbd "C-<delete>"))
                                                        ([?\C-p] . [up])
                                                        ([?\C-n] . [down])
                                                        ([?\C-a] . [home])
                                                        ([?\C-j] . [?\C-k])
                                                        ([?\C-s] . [?\C-f])
                                                        ([?\C-e] . [end])
                                                        ([?\C-E] . [?\C-e])
                                                        ([?\M-v] . [prior])
                                                        ([?\C-v] . [next])
							([?\C-d] . [delete])
                                                        ([?\C-k] . [S-end delete])
                                                        (,(kbd "C-y") . ,(kbd "C-v"))
                                                        (,(kbd "C-x C-x") . ,(kbd "C-x"))
                                                        (,(kbd "C-c C-c") . ,(kbd "C-c")))))))

  (defmacro my/window-switch (direction)
    "Creates a function for changing the focused window but falls
back to switching frames."
    (let ((fn (intern (concat "windmove-" (symbol-name direction)))))
      `(lambda (&optional arg) (interactive)
         (condition-case nil
             (funcall #',fn 1)
           (error (other-frame 1))))))

  (defmacro my/exwm-run (command)
    "Returns a function that calls the given command"
    `(lambda ()
       (interactive)
       (start-process-shell-command ,command nil ,command)))

  (setq exwm-input-global-keys
        `((,(kbd "s-SPC") . ,(my/exwm-run "cabl -c"))
          ([?\s-h] . ,(my/window-switch left))
          ([?\s-l] . ,(my/window-switch right))
          ([?\s-j] . windmove-down)
          ([?\s-k] . windmove-up)
          ;; Window Managment
          (,(kbd "<s-tab>") . other-window)
          ([?\s-v] . crux-swap-windows)
          ([?\s-o] . other-frame)
          ;; ([?\s-f] . exwm-layout-set-fullscreen)
          ([?\s-c] . inferior-octave)
          ([?\s-C] . kill-this-buffer)

          ;; tile exwm
          ([?\s-t] . exwm-reset)

          ;; open a terminal
          (,(kbd "<s-return>") . vterm)
          ;; launch any program
          ([?\s-d] . (lambda (command)
                       (interactive (list (read-shell-command "λ ")))
                       (start-process-shell-command command nil command)))
          ;; Screen And Audio Controls
          (,(kbd "C-s-f")   . ,(my/exwm-run "cm up 5"))
          (,(kbd "C-s-a")   . ,(my/exwm-run "cm down 5"))
          (,(kbd "C-s-d")   . ,(my/exwm-run "xbacklight -inc 10"))
          (,(kbd "C-s-S-d") . ,(my/exwm-run "xbacklight -inc 5"))
          (,(kbd "C-s-s")   . ,(my/exwm-run  "xbacklight -dec 10"))
          (,(kbd "C-s-S-s") . ,(my/exwm-run  "xbacklight -dec 5"))
          ;; Web Browser
          ([?\s-w] . ,(my/exwm-run "ducksearch"))
          ;;Power Manager
          ([?\s-x] . ,(my/exwm-run  "power_menu.sh"))
          ([?\s-m] . (defun remind-timer (reminder)
                       (interactive "reminder?")
                       (egg-timer-do-schedule 3 reminder)))
          ([?\s-=] . ,(my/exwm-run "menu_connection_manager.sh"))
          ([?\s-p] . ,(my/exwm-run "clipmenu"))
          ;; Workspaces
          ([?\s-g] . exwm-workspace-switch)))
  (define-key exwm-mode-map (kbd "C-q") 'exwm-input-send-next-key)
  (define-key exwm-mode-map (kbd "<s-escape>") 'exwm-input-release-keyboard)

  ;; Start in char-mode
  ;; (setq exwm-manage-configurations '((t char-mode t)))

(require 'exwm)
;;;; Start EXWM
;;;; Start Programs For EXWM
(exwm-enable))

;; (use-package exwm-systemtray
;;   :ensure nil
;;   :after exwm
;;   :config
;;   (exwm-systemtray-enable)
;;   (start-process-shell-command "blueman-applet" nil "blueman-applet")
;;   (start-process-shell-command "nm-applet" nil "nm-applet")
;;   (start-process-shell-command "kdeconnect-indicator " nil "kdeconnect-indicator")
;;   (setq exwm-systemtray-height 23))

(use-package exwm-randr
  :ensure nil
  ;; :after exwm
  :demand t
  :config
  (setq exwm-randr-workspace-output-plist ;; '(3 "HDMI2")
        '(2 "DP2"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP1 --primary --auto --right-of DP2 --auto")))
  (exwm-randr-enable))

;;; Streaming
;; Needs OBS-Websockets Plugin
;;   https://obsproject.com/forum/resources/obs-websocket-remote-control-obs-studio-from-websockets.466/
;; Also will need this emacs package
;;   https://github.com/sachac/obs-websocket-el
(defun streaming (&optional)
  "Setup Streaming Environment"
  (interactive)
    (keycast-mode 1)
  (start-process-shell-command "obs " nil "obs")
  (persp-new "demo")
  (let ((spec "streaming"))
    (persp-new spec)
    (persp-switch spec))
  (persp-set-buffer "obs"))
