;; -*- lexical-binding: t; -*-
;;; chatgpt.el --- Simple ChatGPT frontend for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) Gavin Jaeger-Freeborn

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Gavin Jaeger-Freeborn <gavinfreeborn@gmail.com>
;; Maintainer: Gavin Jaeger-Freeborn <gavinfreeborn@gmail.com>
;; Created: 2022
;; Version: 0.34
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Basic commands to use the OpenAI API and use some of the power
;; ChatGPT provides within Emacs.

;; Features include code explanation, attempted code completion,
;; replacing inline queries with the results and more.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup chatgpt nil
  "ChatGPT frontend."
  :group 'convenience
  :prefix "chatgpt-")

(defvar chatgpt-buffer "*TMP-ChatGPT*"
  "Title of the buffer used to store the results of an OpenAI API query.")

(defun chatgpt--append-to-prompt (prompt comment-str)
  "Append the string COMMENT-STR extra information to a PROMPT as a comment."
  (concat comment-str
          "\n"
	  prompt
          ))

(defun chatgpt--display-response (response _)
  (when (stringp response)
    (let ((buf (get-buffer-create chatgpt-buffer)))
      (with-current-buffer buf
        (view-mode -1)
        (erase-buffer)
        (goto-char (point-min))
        (insert response)
        (markdown-mode)
        (view-mode +1))
      (chatgpt-quick-display))))


(defun chatgpt-quick-display ()
  (posframe-show chatgpt-buffer "*TMP-ChatGPT*"
                 :position (point)
                 :hight 100
                 :width 200
                 :cursor t
                 :border-width 2
                 :timeout 10
                 :accept-focus t))

(defun chatgpt-fix-region (BEG END)
  "Takes a region BEG to END asks ChatGPT to explain whats wrong with it.
It then displays the answer in the `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END))
        (gptel-use-tools nil))
    (gptel-request
        (chatgpt--append-to-prompt
         current-code
         "Why doesn't this code work?")
      :callback #'chatgpt--display-response)))

(defun chatgpt-gen-tests-for-region (BEG END)
  "Takes a region BEG to END asks ChatGPT to write a test for it.
It then displays the answer in the `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END))
        (gptel-confirm-tool-calls t))
    (gptel-request
        (chatgpt--append-to-prompt
         current-code
         "Write me a tests for this code")
      :callback #'chatgpt--display-response)))

(defun chatgpt-explain-region (BEG END)
  "Takes a region BEG to END asks ChatGPT what it does.
The answer in the displays in `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END))
        (gptel-use-tools nil))
    (gptel-request
        (chatgpt--append-to-prompt
         current-code
         "What does this code do?")
      :callback #'chatgpt--display-response)))

(defun chatgpt-prompt (prompt)
  "Takes a region BEG to END asks ChatGPT what it does.
The answer in the displays in `chatgpt-buffer'."
  (interactive (list (completing-read "Prompt: " nil)))
  (gptel-request
      prompt
    :callback #'chatgpt--display-response))

;;; Other helpers
(defun chatgpt-generate-commit-message (&optional arg)
  "Generate a simple commit message describing the current git diff"
  (interactive)
  (cl-assert git-commit-mode)
  (require 'gptel)
  (let ((gptel-use-tools nil)
        (gptel-model 'gpt-4o-mini))
    (gptel-request
        (chatgpt--append-to-prompt
         "Write a commit message for this git diff"
         (shell-command-to-string "git diff --cached")))))

(defun chatgpt-send-to-gptel-session (content)
  "Send content to the current *ChatGPT* buffer"
  (interactive "M")
  (let ((m (substring (symbol-name major-mode) 0 -5)))
    (with-current-buffer "*ChatGPT*"
      (goto-char (point-max))
      (insert "\n#+begin_src " m "\n" content "\n#+end_src"))))

(defun my/gptel--edit_buffer (buffer-edits)
  "In the current buffer, apply FILE-EDITS with pattern matching and replacing.
NOTE: Works with narrowed regions which can be helpful"
  (with-temp-message (format "performing edits %s" buffer-edits)
    (let ((old-buf (current-buffer)))
      (with-current-buffer (get-buffer-create "*edit-buffer*")
        (erase-buffer)
        (insert-buffer old-buf)
        (let ((inhibit-read-only t)
              (case-fold-search nil)
              (buf-name (buffer-name old-buf))
              (edit-success nil))
          ;; apply changes
          (dolist (buffer-edit (seq-into buffer-edits 'list))
            (when-let ((line-number (plist-get buffer-edit :line_number))
                       (old-string (plist-get buffer-edit :old_string))
                       (new-string (plist-get buffer-edit :new_string))
                       (is-valid-old-string (not (string= old-string ""))))
              (goto-char (point-min))
              (forward-line (1- line-number))
              (when (search-forward old-string nil t)
                (replace-match new-string t t)
                (setq edit-success t)
                (message "applied %s" new-string))))
          ;; return result to gptel
          (if edit-success
              (progn
                ;; show diffs
                (ediff-buffers old-buf (current-buffer))
                (format "Successfully edited %s" buf-name))
            (format "Failed to edited %s" buf-name)))))))

(setq my/gptel--edit-buffer (gptel-make-tool
                             :function #'my/gptel--edit_buffer
                             :name "edit_buffer"
                             :description "Edit current buffer with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line.
Only use this when explicitly told to."
                             :args (list '(:name "buffer-edits"
                                                 :type array
                                                 :items (:type object
                                                               :properties
                                                               (:line_number
                                                                (:type integer :description "The line number of the buffer where edit starts.")
                                                                :old_string
                                                                (:type string :description "The old-string to be replaced.")
                                                                :new_string
                                                                (:type string :description "The new-string to replace old-string.")))
                                                 :description "The list of edits to apply on the buffer"))
                             :category "buffers"))

(defun chatgpt-grammar-correct ()
  "Correct grammar errors in the current buffer."
  (interactive)
  (let ((gptel-use-tools t)
        (gptel-tools (list my/gptel--edit-buffer)))
    (gptel-request (chatgpt--append-to-prompt
                    (buffer-string)
                    "Use the edit_buffer tool an no other tools to correct the grammar, clarity and spelling in this buffer.
Remember that sentences can cross lines so consider the next line when correcting sentence structure")
      :callback #'ignore
      :system (format "You are a professional writer assisting me with editing my %s text" major-mode))))

(defun chatgpt-add-compilation-to-context ()
  (interactive)
  (with-current-buffer compilation-last-buffer
    (gptel-add)))

(defun chatgpt-add-errors-to-context ()
  (interactive)
  (with-current-buffer (flymake--diagnostics-buffer-name)
    (gptel-add)))

(defun my/python-imports ()
  (treesit-query-capture (treesit-buffer-root-node)
                         '((import_statement name: (_) @import)
                           (import_from_statement module_name: (_)
                                                  name: (_) @name))))
(provide 'chatgpt)
;;; chatgpt.el ends here
