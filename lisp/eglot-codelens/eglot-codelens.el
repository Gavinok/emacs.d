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

(defgroup eglot-lens nil "Code Lens support for eglot."
  :group 'eglot)

(defcustom eglot-lens-debounce 0.001
  "The delay in seconds between file modifications and updating Code Lenses."
  :type 'float
  :group 'eglot-lens)

(defcustom eglot-lens-append-to-line t
  "Determine of Code Lenses are placed above the line or at the end of it."
  :type 'boolean
  :group 'eglot-lens)

;; TODO Make this buffer local
(defvar eglot-lens-overlays nil
  "Code Lens overlays in the current file.")

;; TODO Make this buffer local
(defvar eglot-lens--refresh-timer nil
  "Timer used for debouncing lens refreshes.")

(cl-defmethod eglot-client-capabilities :around (_server)
  "Let the language SERVER know that we support Code Lenses."
  (let ((base (cl-call-next-method)))
    (setf (cl-getf
           (cl-getf base :workspace)
           :codeLens)
          '(:refreshSupport t))
    base))

(defun eglot-lens--setup-hooks ()
  "Setup the hooks to be used for any buffer managed by Eglot."
  (run-with-timer 2 nil #'eglot-delayed-lens-update)
  (add-hook 'eglot-server-initialized-hook #'eglot-delayed-lens-update nil t)
  (add-hook 'eglot--document-changed-hook #'eglot-delayed-lens-update nil t))

(defsubst eglot-lens-delete-overlays ()
  "Clear all the overlays used for Code Lenses."
  (mapc #'delete-overlay eglot-lens-overlays)
  (setq eglot-lens-overlays nil))

(defun eglot-lens-force-refresh-codelens ()
  "Force request and redisplay Code Lenses."
  (interactive)
  (if (eglot-current-server)
      (progn
        (or (eglot-lens-delete-overlays)
            (eglot-lens-apply-code-lenses)))))

(defun eglot-delayed-lens-update (&rest _)
  "Update lenses after a small delay to ensure the server is up to date."
  (when eglot-lens--refresh-timer
    (cancel-timer eglot-lens--refresh-timer))
  (setq eglot-lens--refresh-timer
        (run-with-timer eglot-lens-debounce
                        nil
                        #'(lambda () (eglot-lens-force-refresh-codelens)
                            (setq eglot-lens--refresh-timer nil)))))

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

(defun eglot-lens-make-overlay-for-lens (command range)
  "Insert overlays for each corresponding lens's COMMAND and RANGE."
  (let* ((start-line (thread-first
                       range
                       (cl-getf :start)
                       (cl-getf :line)))
         ;; TODO indent the codelens as needed
         (ol (make-overlay (progn (goto-char (point-min))
                                  (forward-line start-line)
                                  (pos-bol))
                           (pos-eol))))
    (overlay-put ol (if eglot-lens-append-to-line
                        'after-string
                      'before-string)
                 (concat
                  (if eglot-lens-append-to-line
                      "| "
                    (make-string (thread-first
                                   range
                                   (cl-getf :start)
                                   (cl-getf :character))
                                 ?\s))
                  (propertize (cl-getf command :title)
                              'face 'eglot-parameter-hint-face
                              'pointer 'hand
                              'mouse-face 'highlight
                              'keymap (let ((map (make-sparse-keymap)))
                                        (define-key map [mouse-1]
                                                    (lambda () (interactive)
                                                      (eglot-lens-execute command)))
                                        map))
                  (if eglot-lens-append-to-line
                      " "
                    "\n")))
    (overlay-put ol 'eglot-lens-overlay (list :command command
                                              :range range))
    (overlay-put ol 'cursor-face 'error)
    (push ol eglot-lens-overlays)))

(defun eglot-lens-apply-code-lenses ()
  "Request and display code lenses using Eglot."
  ;; TODO look into making this async
  (save-excursion
    (let ((code-lenses
           ;; request code lenses from the server
           (jsonrpc-request (eglot--current-server-or-lose)
                            :textDocument/codeLens
                            (list :textDocument (eglot--TextDocumentIdentifier)))))
      (seq-map (lambda (lens)
                 (eglot-lens-make-overlay-for-lens
                  ;; Construct a lens with a resolved command
                  ;; TODO consider just modifying the lens if that is faster
                  (or (cl-getf lens :command)
                      ;; Resolve the command in case none was originally provided
                      (cl-getf (jsonrpc-request (eglot--current-server-or-lose)
                                                :codeLens/resolve
                                                lens)
                               :command))
                  (cl-getf lens :range)))
               code-lenses))))

(defun eglot-lens-execute (command)
  "Execute a the COMMAND for a specific Code Lens."
  (eglot-execute (eglot--current-server-or-lose)
                 command)
  (eglot-lens-force-refresh-codelens))

(define-minor-mode eglot-lens-mode
  "Minor mode for displaying Code Lenses with Eglot."
  :global t
  ;; TODO handle when (eglot-server-capable :codeLensProvider) is false
  (cond (eglot-lens-mode (add-hook 'eglot-managed-mode-hook #'eglot-lens--setup-hooks)
                         (when eglot--managed-mode (eglot-lens--setup-hooks))
                         (cl-defmethod eglot-handle-request
                           (_server (_method (eql workspace/codeLens/refresh))
                                    &key &allow-other-keys)
                           (eglot-lens-force-refresh-codelens))
                         (advice-add 'eglot--managed-mode-off :after #'eglot-lens-delete-overlays))
        (t (eglot-lens-delete-overlays)
           (remove-hook 'eglot--document-changed-hook #'eglot-delayed-lens-update t)
           (remove-hook 'eglot-managed-mode-hook #'eglot-lens--setup-hooks t)
           (remove-hook 'eglot-server-initialized-hook #'eglot-delayed-lens-update t)
           (advice-remove 'eglot--managed-mode-off #'eglot-lens-delete-overlays))))

(with-eval-after-load 'desktop
  (add-to-list 'desktop-minor-mode-handlers '(eglot-lens-mode . ignore)))

(provide 'eglot-codelens)
;;; eglot-codelens.el ends here
