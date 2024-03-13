;; eglot-codelens.el --- Add support for codelenses to eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; Extending eglot to support lenses

;;;; Findings
;; Lenses often support the option to be used as a code action
;; some servers rely on custom code actions implemented by the client
;; - [[https://github.com/emacs-lsp/lsp-mode/issues/2250]] mentions this


;; TODO
;; - implement the following code actions in for rust analyzer
;;   - rust-analyzer.runSingle
;;   - rust-analyzer.debugSingle
;;   - rust-analyzer.showReferences
;;   - rust-analyzer.triggerParameterHints

(require 'cl-generic)
(require 'cl-lib)
(require 'seq)
(require 'eglot)

(defvar my/eglot-lens-debounce 0.001)
(defvar my/eglot-lens--refresh-timer nil)

(cl-defstruct lens
  command
  range)

(defvar my/eglot-codelens-overlays nil
  "Codelens overlays in the current file.")

(cl-defmethod eglot-client-capabilities :around (_server)
  "Let the language SERVER know that we support codelenses."
  (let ((base (cl-call-next-method)))
    (setf (cl-getf
           (cl-getf base :workspace)
           :codeLens)
          '(:refreshSupport t))
    (message "%s" base)
    base))

(defun eglot-lens--setup-hooks ()
  "Setup the hooks to be used for any buffer managed by eglot."
  (eglot-delayed-lens-update)
  (add-hook 'eglot--document-changed-hook #'eglot-delayed-lens-update nil t))

(defun my/eglot-lens-delete-overlays ()
  "Clear all the overlays used for codelenses."
  (interactive)
  (mapc #'delete-overlay my/eglot-codelens-overlays)
  (setq my/eglot-codelens-overlays nil))

(defun eglot-delayed-lens-update ()
  "Update lenses after a small delay to ensure the server is up to date."
  (setq my/eglot-lens--refresh-timer
        (run-with-timer my/eglot-lens-debounce
                        nil
                        #'my/eglot-force-refresh-codelens)))
(defun my/eglot-get-current-lens ()
  "Inspect the current overlays at point and attempt to execute it."
  (interactive)
  (message "%s" (cl-loop for i in (overlays-at (point))
                         when (overlay-get i 'my/eglot-lens-overlay)
                         collect (overlay-get i 'my/eglot-lens-overlay))))

(defun my/eglot-apply-code-lenses ()
  "Request and display codelenses using eglot"
  (interactive)
  (save-excursion
    (let ((code-lenses
           ;; request code lenses from the server
           (jsonrpc-request (eglot--current-server-or-lose)
                            :textDocument/codeLens
                            (list :textDocument (eglot--TextDocumentIdentifier)))))
      (seq-map (lambda (lens)
                 (my/make-overlay-for-lens
                  (make-lens
                   :command
                   (or (cl-getf lens :command)
                       ;; Resolve the command incase none was originally provided
                       (let ((tmp (jsonrpc-request (eglot--current-server-or-lose)
                                                   :codeLens/resolve
                                                   lens)))
                         (message "%s" tmp)
                         (cl-getf tmp
                                  :command)))
                   :range (cl-getf lens :range))))
               code-lenses))))

(defun my/eglot-force-refresh-codelens ()
  "Force request and redisplay codelenses."
  (interactive)
  (when (eglot-current-server)
    (or (my/eglot-lens-delete-overlays)
        (my/eglot-apply-code-lenses))))

(defun my/eglot-execute-lens (lens)
  "Execute a specific code LENS."
  (eglot-execute (eglot--current-server-or-lose)
                 (lens-command lens))
  (my/eglot-force-refresh-codelens))

(defun my/make-overlay-for-lens (lens)
  "Insert overlays for each corresponding LENS."
  (let* ((start-line (cl-getf
                      (cl-getf (lens-range lens)
                               :start)
                      :line))
         (end-line (cl-getf
                    (cl-getf (lens-range lens)
                             :end)
                    :line))
         ;; TODO indent the codelens as needed
         (ol (make-overlay (progn (goto-char (point-min))
                                  (forward-line start-line)
                                  (pos-bol))
                           (pos-eol))))
    (overlay-put ol 'before-string
                 (concat
                  (propertize (cl-getf (lens-command lens) :title)
                              'face 'eglot-parameter-hint-face
                              'pointer 'hand
                              'mouse-face 'highlight
                              'keymap (let ((map (make-sparse-keymap)))
                                        (define-key map [mouse-1]
                                                    (lambda () (interactive)
                                                      (my/eglot-execute-lens lens)))
                                        map))
                  "\n"))
    (overlay-put ol 'my/eglot-lens-overlay lens)
    (push ol my/eglot-codelens-overlays)))

(define-minor-mode eglot-lens-mode
  "Minor mode for displaying codeLenses with eglot"
  :global t
  (cond (eglot-lens-mode (add-hook 'eglot-managed-mode-hook #'eglot-lens--setup-hooks)
                         (cl-defmethod eglot-handle-notification
                           (_server (_method (eql workspace/codeLens/refresh))
                                    &allow-other-keys)
                           (my/eglot-force-refresh-codelens)))
        (t (remove-hook 'eglot-managed-mode-hook #'eglot-lens--setup-hooks t))))

(provide 'eglot-codelens)
;;; eglot-codelens.el ends here
