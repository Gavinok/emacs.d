(use-package exwm
  :unless my/is-termux
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
  (setq exwm-input-global-keys
        `(([?\s-h] . windmove-left)
          ([?\s-l] . windmove-right)
          ([?\s-j] . other-window)
          ([?\s-k] . (lambda (&optional arg) (other-window -1)))
          ;; Window Managment
          (,(kbd "<s-tab>") . other-window)
          ([?\s-v] . crux-swap-windows)
          ;; ([?\s-v] . crux-transpose-windows)
          ([?\s-o] . my/switch-to-scratch-and-back)
          ([?\s-f] . exwm-layout-set-fullscreen)
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
          ;; screen and audio controls
          (,(kbd "C-s-f") . (lambda ()
                              (interactive)
                              (start-process-shell-command "Vol ↑" nil "cm up 5")))
          (,(kbd "C-s-a") . (lambda ()
                              (interactive)
                              (start-process-shell-command "Vol ↓" nil "cm down 5")))
          (,(kbd "C-s-d") . (lambda ()
                              (interactive)
                              (start-process-shell-command "Brightness ↑" nil "cl up 5")))
          (,(kbd "C-s-s") . (lambda ()
                              (interactive)
                              (start-process-shell-command "Brightness ↓" nil "cl down 5")))
          ;; web browser
          ([?\s-w] . (lambda ()
                       (interactive)
                       (start-process-shell-command "ducksearch" nil "ducksearch")))

          (,(kbd "s-E") . mu4e)
          (,(kbd "s-e") . eshell)
          ;;powermanager
          ([?\s-x] . (lambda ()
                       (interactive)
                       (start-process-shell-command "power_menu.sh" nil "power_menu.sh")))
          ([?\s-m] . (defun remind-timer (reminder)
                       (interactive "reminder?")
                       (egg-timer-do-schedule 3 reminder)))
          ([?\s-=] . (lambda ()
                       (interactive)
                       (start-process-shell-command "Connections" nil
                                                    "menu_connection_manager.sh")))
          ([?\s-p] . (lambda ()
                       (interactive)
                       (start-process-shell-command "Clipmenu" nil "clipmenu")))
;;;; Workspaces
          ([?\s-g] . exwm-workspace-switch)))
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (define-key exwm-mode-map (kbd "<s-escape>") 'exwm-input-release-keyboard)
                                        ;: Start in char-mode
  ;; (setq exwm-manage-configurations '((t char-mode t)))

;;;; Start EXWM
  (exwm-enable)
;;;; Start Programs For EXWM
  ;;   ;; (start-process-shell-command "blueman-applet" nil "blueman-applet")
  (start-process-shell-command "nm-applet" nil "nm-applet")
  (start-process-shell-command "kdeconnect-indicator " nil "kdeconnect-indicator")
  ;; ;;;; Window Divider
  ;;   (setq window-divider-default-right-width 3)
  ;;   (let ((color (face-background 'mode-line)))
  ;;     (dolist (face '(window-divider-first-pixel
  ;;                window-divider-last-pixel
  ;;                window-divider))
  ;;       (set-face-foreground face color)))

  ;;   (window-divider-mode 1)
  ;; ;;;; Mouse Settings
  ;;   :init (setq mouse-autoselect-window t
  ;;          focus-follows-mouse t)
  )
(use-package exwm-systemtray
  :ensure nil
  :after exwm
  :config
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 23))

(use-package exwm-randr
  :ensure nil
  :after exwm
  :defer t
  :config
  (setq exwm-randr-workspace-output-plist '(3 "HDMI2"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP1 --primary --auto --left-of HDMI2 --auto")))
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
