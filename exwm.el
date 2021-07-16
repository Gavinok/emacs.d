;;; EXWM
(use-package exwm
  :unless gv/is-termux
  :config
;;;; Hooks
  (add-hook 'exwm-update-class-hook
	    (lambda ()
	      (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-manage-finish-hook
	    (lambda ()
	      (when (and exwm-class-name
			 (or (string= exwm-class-name "qutebrowser")
			     (string= exwm-class-name "libreoffice-writer")
			     (string= exwm-class-name "libreoffice-calc")
			     (string= exwm-class-name "Google-chrome")
			     (string= exwm-class-name "Brave-browser")))
		(exwm-input-set-local-simulation-keys nil))))

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
(defun gv/switch-to-scratch-and-back ()
    "Toggle between *scratch* buffer and the current buffer.
     If the *scratch* buffer does not exist, create it."
    (interactive)
    (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
        (if (equal (current-buffer) scratch-buffer-name)
            (switch-to-buffer (other-buffer))
            (switch-to-buffer scratch-buffer-name (lisp-interaction-mode)))))

;Just disallow killing of scratch buffer
(defun gv/unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (delete-region (point-min) (point-max))
	      nil)
	  t))
(advice-add 'exwm-reset :after 'consult-buffer-preview)
(add-hook 'kill-buffer-query-functions 'gv/unkillable-scratch-buffer)
;;;; Global Key Bindings
  (setq exwm-input-global-keys
	`(([?\s-h] . windmove-left)
	  ([?\s-l] . windmove-right)
	  ;; Window Managment
	  (,(kbd "<s-tab>") . other-window)
	  ([?\s-v] . crux-swap-windows)
	  ;; ([?\s-v] . crux-transpose-windows)
	  ([?\s-o] . gv/switch-to-scratch-and-back)
	  (,(kbd "s-]")           . edwina-inc-mfact)
	  (,(kbd "s-[")           . edwina-dec-mfact)
	  (,(kbd "s-q")           . edwina-delete-window)
	  (,(kbd "<s-backspace>") . (lambda ()
				      (interactive)
				      (edwina-delete-window)))
	  (,(kbd "<s-return>")    . (lambda ()
				      (interactive)
				      (edwina-clone-window)
				      (balance-windows)
				      (other-window 1)))
	   ([?\s-f] . exwm-layout-set-fullscreen)
	   ([?\s-q] . edwina-delete-window) ; closing windows
	   ([?\s-c] . inferior-octave)
	   ([?\s-C] . (lambda ()
			(interactive)
			(kill-this-buffer)
			(edwina-delete-window)))
	   ;; tile exwm
	   ([?\s-t] . (lambda ()
			(interactive)
			(exwm-reset)
			(edwina-arrange)))

	   ;; open a terminal
	   (,(kbd "s-T") . (lambda ()
			     (interactive)
			     (edwina-clone-window)
				    (vterm)))
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
	   ([?\s-b] . consult-buffer)
	   (,(kbd "C-x C-b") . ibuffer)
	   (,(kbd "s-B") . ibuffer)
	   ([?\s-=] . (lambda ()
			(interactive)
			(start-process-shell-command "Connections" nil
						     "dmenu_connection_manager.sh")))
	   ([?\s-p] . (lambda ()
			(interactive)
			(start-process-shell-command "Clipmenu" nil "clipmenu")))
;;;; Workspaces
	   ([?\s-g] . exwm-workspace-switch)))
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (define-key exwm-mode-map (kbd "<s-escape>") 'exwm-input-release-keyboard)
  ;: Start in char-mode
  (setq exwm-manage-configurations '((t char-mode t)))

  (fringe-mode 1)
;;;; Start EXWM
  (exwm-enable)
;;;; Start Programs For EXWM
  ;; (start-process-shell-command "blueman-applet" nil "blueman-applet")
  (start-process-shell-command "nm-applet" nil "nm-applet")
;;;; Window Divider
  (setq window-divider-default-right-width 3)
  (let ((color (face-background 'mode-line)))
    (dolist (face '(window-divider-first-pixel
		    window-divider-last-pixel
		    window-divider))
      (set-face-foreground face color)))

  (window-divider-mode 1)
;;;; Mouse Settings
  :init (setq mouse-autoselect-window t
	      focus-follows-mouse t)
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
(use-package exwm-mff
  :after exwm
  :config
  (exwm-mff-mode t)) ; mouse follows focus
(use-package edwina
  :after exwm
  :config
  ;;(setq display-buffer-base-action '(display-buffer-below-selected))
  ) ; dwm style window managment
(use-package vlf
  :ensure t
  :defer t) ; help emacs handle large files to avoid exwm from locking
