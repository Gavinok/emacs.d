;;; /home/gavinok/.emacs.d/lisp/exwm-config.el -- My exwm config -*- lexical-binding: t; 
;;; Commentary:
;;; Code:

(use-package exwm
  :unless my/is-termux
  :ensure t
  :init
  (setopt exwm-manage-force-tiling t)
  (setopt exwm-workspace-number 2
          mouse-autoselect-window t
          focus-follows-mouse t)
  :config
 ;;;; Hooks
  (defun my/exwm-titles ()
    (pcase exwm-class-name
      ("Brave-browser" (exwm-workspace-rename-buffer (format "%s" exwm-title)))
      ("qutebrowser" (exwm-workspace-rename-buffer (format "%s" exwm-title)))
      ("mpv" (exwm-workspace-rename-buffer (format "%s" exwm-title)))
      ("libreoffice-writer" (exwm-workspace-rename-buffer (format "Writer: %s" exwm-title)))
      ("libreoffice-calc" (exwm-workspace-rename-buffer (format "Calc: %s" exwm-title)))
      ("St" (exwm-workspace-rename-buffer (format "%s" exwm-title)))))

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-update-title-hook #'my/exwm-titles)

  ;; Hide the mode-line on all floating X windows
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (exwm-layout-hide-mode-line)))
  ;; Functions
  ;; Global Key Bindings
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (when (and exwm-class-name
                         (or (string= exwm-class-name "Google-chrome")
                             (string= exwm-class-name "discord")))
                (exwm-input-set-local-simulation-keys `((,(kbd "C-S-E") . [?\C-e]))))))

  (defmacro my/window-switch (direction)
    "Creates a function for changing the focused window but falls
back to switching frames."
    (let ((fn (intern (concat "windmove-" (symbol-name direction)))))
      `(lambda (&optional arg) (interactive)
         (condition-case nil
             (funcall #',fn 1)
           (error (other-frame 1))))))

  (defmacro my/window-swap (direction)
    "Creates a function for changing the focused window but falls
back to switching frames."
    (let ((fn (intern (concat "windmove-swap-states-" (symbol-name direction)))))
      `(lambda (&optional arg) (interactive)
         (condition-case nil
             (funcall #',fn)
           (error (other-frame 1))))))

  (defmacro my/exwm-run (command)
    "Returns a function that calls the given command"
    `(lambda (&optional arg)
       (interactive)
       (start-process-shell-command ,command nil ,command)))
  (cl-defmacro exwm-bind (&rest pairs)
    (cl-loop for (key . func) in pairs
             collect `(cons (kbd ,key) . (quote ,func))))
  (keymap-global-set "s-j" 'other-window)
  (keymap-global-set "s-k" 'other-other-window)
  (defun exwm-dmenu (command)
    (interactive (list (read-shell-command "λ ")))
    (start-process-shell-command command nil command))
  (defun my/dwm-like-next-window (&optional arg)
    (interactive "p")
    (message "arg was %s" arg)
    (let* ((dir-order (if (< 0 arg)
                          (list 'down 'right 'up)
                        (list 'up 'left 'down)))
           (next-wind (windmove-find-other-window (cl-first dir-order))))
      (if (and (window-buffer next-wind)
               (minibufferp (window-buffer next-wind)))
          (cl-loop with x = (if (windmove-find-other-window (cl-second dir-order))
                                (windmove-right)
                              (windmove-left))
                   for i = (windmove-find-other-window (cl-third dir-order))
                   while (and i
                              (not (minibufferp (window-buffer i))))
                   do (windmove-do-window-select (cl-third dir-order)))
        (windmove-do-window-select (cl-first dir-order)))))
  (setq exwm-input-global-keys
        ;; Window Managment
        `((,(kbd "s-SPC")   . ,(my/exwm-run "cabl -c"))
          ([?\s-h]          . ,(my/window-switch left))
          ([?\s-l]          . ,(my/window-switch right))
          ([?\s-j]          . other-window)
          ([?\s-k]          . other-other-window)
          (,(kbd "s-H")     . windmove-swap-states-left)
          (,(kbd "s-L")     . ,(my/window-swap right))
          (,(kbd "s-J")     . ,(my/window-swap down))
          (,(kbd "s-K")     . ,(my/window-swap up))
          (,(kbd "<s-tab>") . other-window)
          ([?\s-v]          . crux-swap-windows)
          ([?\s-o]          . other-frame)
          ([?\s-f]          . exwm-layout-toggle-fullscreen)
          ([?\s-c]          . inferior-octave)
          ([?\s-C]          . kill-this-buffer)
          ;; tile exwm
          ([?\s-t] . exwm-reset)

          ;; open a terminal
          (,(kbd "<s-return>") . vterm)
          ;; launch any program
          ([?\s-d] . exwm-dmenu)
          ;; Screen And Audio Controls
          (,(kbd "C-s-f")   . ,(my/exwm-run "cm up 5"))
          (,(kbd "C-s-a")   . ,(my/exwm-run "cm down 5"))
          (,(kbd "C-s-d")   . ,(my/exwm-run "xbacklight -inc 10"))
          (,(kbd "C-s-S-d") . ,(my/exwm-run "xbacklight -inc 5"))
          (,(kbd "C-s-s")   . ,(my/exwm-run "xbacklight -dec 10"))
          (,(kbd "C-s-S-s") . ,(my/exwm-run "xbacklight -dec 5"))
          ;; Web Browser
          ([?\s-w] . ,(my/exwm-run "ducksearch"))
          ;;Power Manager
          ([?\s-x] . ,(my/exwm-run  "power_menu.sh"))
          ([?\s-=] . ,(my/exwm-run "menu_connection_manager.sh"))
          (,(kbd "s-P") . ,(my/exwm-run "clipmenu"))
          ;; Workspaces
          ([?\s-g] . exwm-workspace-switch)))
  (keymap-set exwm-mode-map "C-q" 'exwm-input-send-next-key)

  (require 'exwm)
  ;; Ensure that we can pass C-c and C-x through to programs
  (exwm-input--set-simulation-keys `((,(kbd "C-x C-x") . ,(kbd "C-x"))
                                     (,(kbd "C-c C-c") . ,(kbd "C-c"))))

  
  ;; Start EXWM
  ;; Start Programs For EXWM
  (exwm-enable))

(with-eval-after-load 'corfu
  (defun get-focused-monitor-geometry ()
    "Get the geometry of the monitor displaying the selected frame in EXWM."
    (let* ((monitor-attrs (frame-monitor-attributes))
           (workarea (assoc 'workarea monitor-attrs))
           (geometry (cdr workarea)))
      (list (nth 0 geometry) ; X
            (nth 1 geometry) ; Y
            (nth 2 geometry) ; Width
            (nth 3 geometry) ; Height
            )))
  (defun advise-corfu-make-frame-with-monitor-awareness (orig-fun frame x y width height buffer)
    "Advise `corfu--make-frame` to be monitor-aware, adjusting X and
Y according to the focused monitor."

    ;; Get the geometry of the currently focused monitor
    (let* ((monitor-geometry (get-focused-monitor-geometry))
           (monitor-x (nth 0 monitor-geometry))
           (monitor-y (nth 1 monitor-geometry))
           ;; You may want to adjust the logic below if you have specific preferences
           ;; on where on the monitor the posframe should appear.
           ;; Currently, it places the posframe at its intended X and Y, but ensures
           ;; it's within the bounds of the focused monitor.
           (new-x (+ monitor-x x))
           (new-y (+ monitor-y y)))

      ;; Call the original function with potentially adjusted coordinates
      (funcall orig-fun frame new-x new-y width height buffer)))

  (advice-add 'corfu--make-frame :around #'advise-corfu-make-frame-with-monitor-awareness))

;; Broken on current version of emacs
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
  :demand t
  :config
  (setopt exwm-randr-workspace-monitor-plist '(1 "DVI-I-1-1"))
  (defun exwm-randr-setup ()
    (lambda ()
      (start-process-shell-command
       "xrandr" nil "xrandr --output eDP1 --primary --auto --right-of DVI-I-1-1 --auto")))


  ;;   (setopt exwm-randr-workspace-monitor-plist '(1 "dp2-1" 3 "dp2-3"))
  ;;   (defun exwm-randr-setup ()
  ;;     (lambda ()
  ;;       (start-process-shell-command
  ;;        "xrandr" nil "xrandr --output edp1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output dp1 --off --output dp2 --off --output dp2-1 --mode 1920x1080 --pos 0x0 --rotate normal --output dp2-2 --off --output dp2-3 --mode 1920x1080 --pos 3840x0 --rotate normal --output hdmi1 --off --output hdmi2 --off --output virtual1 --off --output dvi-i-1-1 --off
  ;; ")))
  (add-hook 'exwm-randr-screen-change-hook 'exwm-randr-setup)
  (exwm-randr-enable))

;;; Streaming
;; Needs OBS-Websockets Plugin
;;   https://obsproject.com/forum/resources/obs-websocket-remote-control-obs-studio-from-websockets.466/
;; Also will need this emacs package
;;   https://github.com/sachac/obs-websocket-el

(defvar-keymap exwm-prefix-map
  :doc "My prefix for use with EXWM"
  "k" #'kill-current-buffer
  "c" #'eat
  "o" #'other-window
  "SPC" (my/exwm-run "cabl -c"))
(keymap-global-set "<XF86Tools>" exwm-prefix-map)
(keymap-set exwm-mode-map "<XF86Tools>" exwm-prefix-map)

(defun other-other-frame ()
  (interactive)
  (other-frame -1))
(keymap-global-set "s-l" 'other-frame)
(keymap-global-set "s-h" 'other-other-frame)

;;; Override Pinentry For Pass To Avoid Locking Up Emacs
(use-package pinentry
  :ensure t
  :init
  :config
  ;; let's get encryption established
  (setenv "GPG_AGENT_INFO" nil)  ;; use emacs pinentry
  (setq auth-source-debug t)

  (setq epg-gpg-program "gpg2")  ;; not necessary
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start)

  (require 'org-crypt)
  ;; Add the following to ~/.gnupg/gpg-agent.conf
  ;; allow-emacs-pinentry
  ;; allow-loopback-pinentry
  (org-crypt-use-before-save-magic))
(use-package frame
  :config
  (setopt window-divider-default-right-width 3)
  (setopt window-divider-default-places t)
  (window-divider-mode 1))

;;; exwm-config.el ends here

