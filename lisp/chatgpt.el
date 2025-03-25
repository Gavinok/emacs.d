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

(defvar chatgpt-buffer "*ChatGPT*"
  "Title of the buffer used to store the results of an OpenAI API query.")

(defun chatgpt--append-to-prompt (prompt comment-str)
  "Append the string COMMENT-STR extra information to a PROMPT as a comment."
  (concat prompt
          "\n"
	  comment-start
          " "
	  comment-str))

(defun chatgpt--display-response (response _)
  (when (stringp response)
    (let ((buf (get-buffer-create chatgpt-buffer)))
      (with-current-buffer buf
        (view-mode -1)
        (erase-buffer)
        (goto-char 0)
        (insert response)
        (markdown-mode)
        (view-mode +1))
      (switch-to-buffer-other-window chatgpt-buffer))))

(defun chatgpt-fix-region (BEG END)
  "Takes a region BEG to END asks ChatGPT to explain whats wrong with it.
It then displays the answer in the `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END)))
    (gptel-request
        (chatgpt--append-to-prompt
         current-code
         "Why doesn't this code work?")
      :callback #'chatgpt--display-response)))

(defun chatgpt-gen-tests-for-region (BEG END)
  "Takes a region BEG to END asks ChatGPT to write a test for it.
It then displays the answer in the `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END)))
    (gptel-request
        (chatgpt--append-to-prompt
         current-code
         "Write me a tests for this code")
      :callback #'chatgpt--display-response)))

(defun chatgpt-explain-region (BEG END)
  "Takes a region BEG to END asks ChatGPT what it does.
The answer in the displays in `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END)))
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
(provide 'chatgpt)
;;; chatgpt.el ends here
