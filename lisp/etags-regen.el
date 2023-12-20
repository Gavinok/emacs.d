;;; etags-regen.el --- Auto-(re)regenerating tags  -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2023 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dmitry@gutov.dev>
;; Keywords: tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple tags generation with automatic invalidation.

;;; Code:

(require 'cl-lib)

(defgroup etags-regen nil
  "Auto-(re)generating tags."
  :group 'tools)

(defvar etags-regen--tags-file nil)
(defvar etags-regen--tags-root nil)
(defvar etags-regen--new-file nil)

(declare-function project-root "project")
(declare-function project-files "project")

(defcustom etags-regen-program (executable-find "etags")
  "Name of the etags executable."
  ;; Always having our 'etags' here would be easier, but we can't
  ;; always rely on it being installed.  So it might be ctags's etags.
  :type 'file)

(defcustom etags-regen-tags-file "TAGS"
  "Name of the tags file to create inside the project."
  :type 'string)

(defcustom etags-regen-program-options nil
  "List of additional options to pass to the etags program."
  :type '(repeat string))

(defcustom etags-regen-lang-regexp-alist nil
  "Mapping of languages to additional regexps for tags.

Each language should be one of the recognized by etags, see
`etags --help'.  Each tag regexp should be a string in the format
as documented for the `--regex' arguments.

We support only Emacs's etags program with this option."
  :type '(repeat
          (cons
           :tag "Languages group"
           (repeat (string :tag "Language name"))
           (repeat (string :tag "Tag Regexp")))))

;;;###autoload
(put 'etags-regen-lang-regexp-alist 'safe-local-variable
     (lambda (value)
       (and (listp value)
            (seq-every-p
             (lambda (group)
               (and (consp group)
                    (listp (car group))
                    (listp (cdr group))
                    (seq-every-p
                     (lambda (lang)
                       (and (stringp lang)
                            (string-match-p "\\`[a-z*+]+\\'" lang)))
                     (car group))
                    (seq-every-p #'stringp (cdr group))))
             value))))

;; XXX: We have to list all extensions: etags falls back to Fortran.
;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00323.html
(defcustom etags-regen-file-extensions
  '("rb" "js" "py" "pl" "el" "c" "cpp" "cc" "h" "hh" "hpp"
    "java" "go" "cl" "lisp" "prolog" "php" "erl" "hrl"
    "F" "f" "f90" "for" "cs" "a" "asm" "ads" "adb" "ada")
  "Code file extensions.

File extensions to generate the tags for."
  :type '(repeat (string :tag "File extension")))

;;;###autoload
(put 'etags-regen-file-extensions 'safe-local-variable
     (lambda (value)
       (and (listp value)
            (seq-every-p
             (lambda (ext)
               (and (stringp ext)
                    (string-match-p "\\`[a-zA-Z0-9]+\\'" ext)))
             value))))

;; FIXME: Only plain substrings supported currently.
(defcustom etags-regen-ignores nil
  "Additional ignore rules, in the format of `project-ignores'."
  :type '(repeat
          (string :tag "Glob to ignore")))

;;;###autoload
(put 'etags-regen-ignores 'safe-local-variable
     (lambda (value)
       (and (listp value)
            (seq-every-p #'stringp value))))

(defvar etags-regen--errors-buffer-name "*etags-regen-tags-errors*")

(defun etags-regen--all-mtimes (proj)
  (let ((files (etags-regen--all-files proj))
        (mtimes (make-hash-table :test 'equal))
        file-name-handler-alist)
    (dolist (f files)
      (condition-case nil
          (puthash f
                   (file-attribute-modification-time
                    (file-attributes f))
                   mtimes)
        (file-missing nil)))
    mtimes))

(defun etags-regen--refresh (proj)
  (save-excursion
    (let* ((tags-file (expand-file-name etags-regen-tags-file
                                        (project-root proj)))
           (tags-mtime (file-attribute-modification-time
                        (file-attributes tags-file)))
           (all-mtimes (etags-regen--all-mtimes proj))
           added-files
           changed-files
           removed-files)
      (etags-regen--visit-table tags-file (project-root proj))
      (set-buffer (get-file-buffer tags-file))
      (dolist (file (tags-table-files))
        (let ((mtime (gethash file all-mtimes)))
          (cond
           ((null mtime)
            (push file removed-files))
           ((time-less-p tags-mtime mtime)
            (push file changed-files)
            (remhash file all-mtimes))
           (t
            (remhash file all-mtimes)))))
      (maphash
       (lambda (key _value)
         (push key added-files))
       all-mtimes)
      (if (> (+ (length added-files)
                (length changed-files)
                (length removed-files))
             100)
          (progn
            (message "etags-regen: Too many changes, falling back to full rescan")
            (etags-regen--tags-cleanup))
        (dolist (file (nconc removed-files changed-files))
          (etags-regen--remove-tag file))
        (when (or changed-files added-files)
          (apply #'etags-regen--append-tags
                 (nconc changed-files added-files)))
        (when (or changed-files added-files removed-files)
          (let ((save-silently t)
                (message-log-max nil))
            (save-buffer 0)))))))

(defun etags-regen--maybe-generate ()
  (let ((proj))
    (when (and etags-regen--tags-root
               (not (file-in-directory-p default-directory
                                         etags-regen--tags-root)))
      (etags-regen--tags-cleanup))
    (when (and (not etags-regen--tags-root)
               (file-exists-p (expand-file-name
                               etags-regen-tags-file
                               (project-root
                                (setq proj (project-current))))))
      (message "Found existing tags table, refreshing...")
      (etags-regen--refresh proj))
    (when (and (not (or tags-file-name
                        tags-table-list))
               (setq proj (or proj (project-current))))
      (message "Generating new tags table...")
      (let ((start (time-to-seconds)))
        (etags-regen--tags-generate proj)
        (message "...done (%.2f s)" (- (time-to-seconds) start))))))

(defun etags-regen--all-files (proj)
  (let* ((root (project-root proj))
         (default-directory root)
         ;; TODO: Make the scanning more efficient, e.g. move the
         ;; filtering by glob to project (project-files-filtered...).
         (files (project-files proj))
         (match-re (concat
                    "\\."
                    (regexp-opt etags-regen-file-extensions)
                    "\\'")))
    (cl-delete-if
     (lambda (f) (or (not (string-match-p match-re f))
                     ;; FIXME: Handle etags-regen-ignores properly.
                     (string-match-p "/\\.#" f)
                     (cl-some (lambda (ignore) (string-search ignore f))
                              etags-regen-ignores)))
     files)))

(defun etags-regen--tags-generate (proj)
  (require 'dired)
  (let* ((root (project-root proj))
         (default-directory root)
         (files (etags-regen--all-files proj))
         (tags-file (expand-file-name etags-regen-tags-file root))
         (ctags-p (etags-regen--ctags-p))
         (command (format "%s %s %s - -o %s"
                          etags-regen-program
                          (mapconcat #'identity
                                     (etags-regen--build-program-options ctags-p)
                                     " ")
                          ;; ctags's etags requires '-L' for stdin input.
                          (if ctags-p "-L" "")
                          tags-file)))
    (with-temp-buffer
      (mapc (lambda (f)
              (insert f "\n"))
            files)
      (shell-command-on-region (point-min) (point-max) command
                               nil nil etags-regen--errors-buffer-name t))
    (etags-regen--visit-table tags-file root)))

(defun etags-regen--visit-table (tags-file root)
  ;; Invalidate the scanned tags after any change is written to disk.
  (add-hook 'after-save-hook #'etags-regen--update-file)
  (add-hook 'before-save-hook #'etags-regen--mark-as-new)
  (setq etags-regen--tags-file tags-file
        etags-regen--tags-root root)
  (visit-tags-table etags-regen--tags-file))

(defun etags-regen--ctags-p ()
  (string-search "Ctags"
                 (shell-command-to-string
                  (format "%s --version" etags-regen-program))))

(defun etags-regen--build-program-options (ctags-p)
  (when (and etags-regen-lang-regexp-alist ctags-p)
    (user-error "etags-regen-lang-regexp-alist is not supported with Ctags"))
  (nconc
   (mapcan
    (lambda (group)
      (mapcan
       (lambda (lang)
         (mapcar (lambda (regexp)
                   (concat "--regex="
                           (shell-quote-argument
                            (format "{%s}%s" lang regexp))))
                 (cdr group)))
       (car group)))
    etags-regen-lang-regexp-alist)
   etags-regen-program-options))

(defun etags-regen--update-file ()
  ;; TODO: Maybe only do this when Emacs is idle for a bit.  Or defer
  ;; the updates and do them later in bursts when the table is used.
  (let ((file-name buffer-file-name)
        (tags-file-buf (get-file-buffer etags-regen--tags-file))
        pr should-scan)
    (save-excursion
      (when tags-file-buf
        (cond
         ((and etags-regen--new-file
               (kill-local-variable 'etags-regen--new-file)
               (setq pr (project-current))
               (equal (project-root pr) etags-regen--tags-root)
               (member file-name (project-files pr)))
          (set-buffer tags-file-buf)
          (setq should-scan t))
         ((progn (set-buffer tags-file-buf)
                 (etags-regen--remove-tag file-name))
          (setq should-scan t))))
      (when should-scan
        (etags-regen--append-tags file-name)
        (let ((save-silently t)
              (message-log-max nil))
          (save-buffer 0))))))

(defun etags-regen--remove-tag (file-name)
  (goto-char (point-min))
  (when (search-forward (format "\f\n%s," file-name) nil t)
    (let ((start (match-beginning 0)))
      (search-forward "\f\n" nil 'move)
      (let ((inhibit-read-only t))
        (delete-region start
                       (if (eobp)
                           (point)
                         (- (point) 2)))))
    t))

(defun etags-regen--append-tags (&rest file-names)
  (goto-char (point-max))
  (let ((options (etags-regen--build-program-options (etags-regen--ctags-p)))
        (inhibit-read-only t))
    ;; FIXME: call-process is significantly faster, though.
    ;; Like 10ms vs 20ms here.
    (shell-command
     (format "%s %s %s -o -"
             etags-regen-program (mapconcat #'identity options " ")
             (mapconcat #'identity file-names " "))
     t etags-regen--errors-buffer-name))
  ;; FIXME: Is there a better way to do this?
  ;; Completion table is the only remaining place where the
  ;; update is not incremental.
  (setq-default tags-completion-table nil))

(defun etags-regen--mark-as-new ()
  (unless buffer-file-number
    (setq-local etags-regen--new-file t)))

(defun etags-regen--tags-cleanup ()
  (when etags-regen--tags-file
    (let ((buffer (get-file-buffer etags-regen--tags-file)))
      (and buffer
           (kill-buffer buffer)))
    (setq tags-file-name nil
          tags-table-list nil
          etags-regen--tags-file nil
          etags-regen--tags-root nil))
  (remove-hook 'after-save-hook #'etags-regen--update-file)
  (remove-hook 'before-save-hook #'etags-regen--mark-as-new))

;;;###autoload
(define-minor-mode etags-regen-mode
  "Generate tags automatically."
  :global t
  (if etags-regen-mode
      (progn
        (advice-add 'etags--xref-backend :before
                    #'etags-regen--maybe-generate)
        (advice-add 'tags-completion-at-point-function :before
                    #'etags-regen--maybe-generate))
    (advice-remove 'etags--xref-backend #'etags-regen--maybe-generate)
    (advice-remove 'tags-completion-at-point-function #'etags-regen--maybe-generate)
    (etags-regen--tags-cleanup)))

(provide 'etags-regen)

;;; etags-regen.el ends here
