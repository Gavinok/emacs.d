;;; purescript-flymake.el --- A purescript Flymake backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Gavin Jaeger-Freeborn

;; Author: Gavin Jaeger-Freeborn <gavinfreeborn@gmail.com>
;; Keywords: flymake
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: Flymake backend using purescript's spago build command.

;;; Code:
(require 'flymake)

(defvar-local purescript--flymake-proc nil
  "Variable storing the currently active flymake process running the purescript checker.
This is used to determine if the current diagnostics are out of date.")

;;;###autoload
(defun purescript-flymake (report-fn &rest _args)
  "Flymake backend for Purescript syntax check.
REPORT-FN is Flymake’s callback function."
  ;; If the purescript compiler is not installed signal an error
  (unless (executable-find "spago")
    (error "Cannot find a suitable version of the spago compiler"))
  ;; If there is already a check runnning kill it
  (when (process-live-p purescript--flymake-proc)
    (kill-process purescript--flymake-proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  ;;
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `purescript--flymake-proc' process to a new process
      ;; calling the purescript tool.
      ;;
      (let ((default-directory (project-root (project-current))))
        (setq purescript--flymake-proc
	      (make-process
	       :name "purescript-flymake" :noquery t :connection-type 'pipe
	       ;; Make output go to a temporary buffer.
	       :buffer (generate-new-buffer "*purescript-flymake*")
	       :command (list "spago" "build")
	       :sentinel
	       (lambda (proc _event)
	         ;; Check that the process has indeed exited, as it might
	         ;; be simply suspended.
	         (when (memq (process-status proc) '(exit signal))
		   (unwind-protect
		       ;; Only proceed if `proc' is the same as
		       ;; `purescript--flymake-proc', which indicates that
		       ;; `proc' is not an obsolete process.
		       ;;
		       (if (with-current-buffer source (eq proc purescript--flymake-proc))
			   (with-current-buffer (process-buffer proc)
			     (goto-char (point-min))
			     ;; Parse the output buffer for diagnostics's
			     ;; messages and locations, collect them in a list
			     ;; of objects, and call `report-fn'.

			     (cl-loop
			      while (search-forward-regexp
                                     ;; "^\\(Warning\\|Error\\).*:
                                     ;; ?
                                     ;; .*
                                     ;; .*at \\(.+\\|-\\):\\([0-9]+\\):\\([0-9]+\\).*

                                     ;; \\(\\s-.*
                                     ;; \\|
                                     ;; \\)+"
                                     "^\\(Warning\\|Error\\).*:\n\?\n.*\n.*at \\(.+\\|-\\):\\([0-9]+\\):\\([0-9]+\\).*\n\n\\s-+\\(.*\\)$"
				     nil t)
                              for source = (get-file-buffer (match-string 2))
			      for msg = (match-string 5)
			      for (beg . end) = (flymake-diag-region
					         source
					         (string-to-number (match-string 3)))
			      for type = (if (string= "Warning" (match-string 1))
                                             :warning
                                           :error)
			      collect (flymake-make-diagnostic source
							       beg
							       end
							       type
							       msg)
			      into diags
			      finally (funcall report-fn diags)))
		         (flymake-log :warning "Canceling obsolete check %s"
				      proc))
		     ;; Cleanup the temporary buffer used to hold the
		     ;; check's output.
		     ;; (kill-buffer (process-buffer proc))
                     )))))))))


(defun purescript-setup-flymake-backend ()
  "Add the flymake-backend to the `flymake-diagnostic-functions'."
  (add-hook 'flymake-diagnostic-functions 'purescript-flymake nil t))

(provide 'purescript-flymake)
;;; purescript-flymake.el ends here
