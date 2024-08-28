;;;; Use emacs instead of dmenu
(defun emenu (prompt options)
  "Create and select a frame called emacs-run-launcher which
consists only of a minibuffer and has specific dimensions. Run
the prompt given will be used and the user will be displayed a
dmenu like interface"
  (interactive)
  (let ((tmp-frame (make-frame '((name . "emacs-run-launcher")
				 (minibuffer . only)
				 (width . 120)
				 (height . 11)))))
    (with-selected-frame tmp-frame
      (unwind-protect
	  (completing-read prompt options)
        (delete-frame tmp-frame)))))

(setenv "LAUNCHER" "emenu -p ")
