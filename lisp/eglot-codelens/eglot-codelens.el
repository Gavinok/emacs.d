;; eglot-codelens.el --- Add support for codelenses to eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; Extending eglot to support lenses

;;;; Findings
;; Lenses often support the option to be used as a code action
;; some servers rely on custom code actions implemented by the client
;; - [[https://github.com/emacs-lsp/lsp-mode/issues/2250]] mentions this


;; TODO
;; - Ensure lenses are cleand up when a server is shutdown
;; - implement the following code actions in for rust analyzer
;;   - rust-analyzer.runSingle
;;   - rust-analyzer.debugSingle
;;   - rust-analyzer.showReferences
;;   - rust-analyzer.triggerParameterHints

(require 'cl-generic)
(require 'cl-lib)
(require 'seq)
(require 'eglot)

(defgroup eglot-lens nil "Codelens support for eglot"
  :group 'eglot)

(defcustom eglot-lens-debounce 0.001
  "The delay in seconds between file modifications and updating codelenses."
  :type 'float
  :group 'eglot-lens)

(cl-defstruct eglot-lens-lens
  command
  range)

(defvar eglot-lens-overlays nil
  "Codelens overlays in the current file.")

(defvar eglot-lens--refresh-timer nil
  "Timer used for debouncing lens refreshes")

(cl-defmethod eglot-client-capabilities :around (_server)
  "Let the language SERVER know that we support codelenses."
  (let ((base (cl-call-next-method)))
    (setf (cl-getf
           (cl-getf base :workspace)
           :codeLens)
          '(:refreshSupport t))
    base))

(defun eglot-lens--setup-hooks ()
  "Setup the hooks to be used for any buffer managed by eglot."
  (eglot-delayed-lens-update)
  (add-hook 'eglot--document-changed-hook #'eglot-delayed-lens-update nil t))

(defun eglot-lens-delete-overlays ()
  "Clear all the overlays used for codelenses."
  (mapc #'delete-overlay eglot-lens-overlays)
  (setq eglot-lens-overlays nil))

(defun eglot-lens-force-refresh-codelens ()
  "Force request and redisplay codelenses."
  (interactive)
  (when (eglot-current-server)
    (or (eglot-lens-delete-overlays)
        (eglot-lens-apply-code-lenses))))

(defun eglot-delayed-lens-update ()
  "Update lenses after a small delay to ensure the server is up to date."
  (setq eglot-lens--refresh-timer
        (run-with-timer eglot-lens-debounce
                        nil
                        #'eglot-lens-force-refresh-codelens)))

(defun eglot-lens-execute-current-lens ()
  "Inspect the current overlays at point and attempt to execute it."
  (interactive)
  (let ((lenses (cl-loop for i in (overlays-at (point))
                         when (overlay-get i 'eglot-lens-overlay)
                         collect (overlay-get i 'eglot-lens-overlay))))
    (pcase (length lenses)
      (1 (eglot-lens-execute (car lenses)))
      (0 (user-error "No lenses found"))
      (lenses (user-error "Too many lenses %s" lenses)))))

(defun eglot-lens-make-overlay-for-lens (lens)
  "Insert overlays for each corresponding LENS."
  (let* ((append-to-line t)
         (start-line (thread-first
                       (eglot-lens-lens-range lens)
                       (cl-getf :start)
                       (cl-getf :line)))
         ;; TODO indent the codelens as needed
         (ol (make-overlay (progn (goto-char (point-min))
                                  (forward-line start-line)
                                  (pos-bol))
                           (pos-eol))))
    (overlay-put ol (if append-to-line
                        'after-string
                      'before-string)
                 (concat
                  (if append-to-line
                      "| "
                    (make-string (thread-first
                                   (eglot-lens-lens-range lens)
                                   (cl-getf :start)
                                   (cl-getf :character))
                                 ?\s))
                  (propertize (cl-getf (eglot-lens-lens-command lens) :title)
                              'face 'eglot-parameter-hint-face
                              'pointer 'hand
                              'mouse-face 'highlight
                              'keymap (let ((map (make-sparse-keymap)))
                                        (define-key map [mouse-1]
                                                    (lambda () (interactive)
                                                      (eglot-lens-execute lens)))
                                        map))
                  (if append-to-line
                      " "
                    "\n")))
    (overlay-put ol 'eglot-lens-overlay lens)
    (overlay-put ol 'cursor-face 'error)
    (push ol eglot-lens-overlays)))

(defun eglot-lens-apply-code-lenses ()
  "Request and display codelenses using eglot"
  (save-excursion
    (let ((code-lenses
           ;; request code lenses from the server
           (jsonrpc-request (eglot--current-server-or-lose)
                            :textDocument/codeLens
                            (list :textDocument (eglot--TextDocumentIdentifier)))))
      (seq-map (lambda (lens)
                 (eglot-lens-make-overlay-for-lens
                  (make-eglot-lens-lens
                   :command
                   (or (cl-getf lens :command)
                       ;; Resolve the command incase none was originally provided
                       (cl-getf (jsonrpc-request (eglot--current-server-or-lose)
                                                 :codeLens/resolve
                                                 lens)
                                :command))
                   :range (cl-getf lens :range))))
               code-lenses))))

(defun eglot-lens-execute (lens)
  "Execute a specific code LENS."
  (eglot-execute (eglot--current-server-or-lose)
                 (eglot-lens-lens-command lens))
  (eglot-lens-force-refresh-codelens))


(define-minor-mode eglot-lens-mode
  "Minor mode for displaying codeLenses with eglot"
  :global t
  (cond (eglot-lens-mode (add-hook 'eglot-managed-mode-hook #'eglot-lens--setup-hooks)
                         (when eglot--managed-mode (eglot-lens--setup-hooks))
                         (cl-defmethod eglot-handle-notification
                           (_server (_method (eql workspace/codeLens/refresh))
                                    &allow-other-keys)
                           (eglot-lens-force-refresh-codelens)))
        (t (eglot-lens-delete-overlays)
           (remove-hook 'eglot--document-changed-hook #'eglot-delayed-lens-update t)
           (remove-hook 'eglot-managed-mode-hook #'eglot-lens--setup-hooks t))))

(provide 'eglot-codelens)
;;; eglot-codelens.el ends here
