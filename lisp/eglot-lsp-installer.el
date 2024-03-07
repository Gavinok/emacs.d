;; -*- lexical-binding: t -*-
(require 'ansi-color)
(require 'cl-lib)
;; TODO integrate with https://mason-registry.dev/registry/list / https://github.com/mason-org/mason-registry
;; TODO Write a simple https://github.com/package-url/purl-spec parser
;; TODO How mason handles the conversion https://github.com/williamboman/mason.nvim/blob/41e75af1f578e55ba050c863587cffde3556ffa6/lua/mason-registry/api.lua
;; TODO Setup path to point to paths as needed
;; TODO Auto update this path after a new lsp is installed
;; TODO add eshell support seen in https://github.com/purcell/envrc/blob/master/envrc.el
;; TODO Add a minor mode for updating the exec-path
;; TODO Define a proper format rather than a simple function returning a record
;; TODO Only install if needed
;; TODO how to update (optional)

(defun my/eglot-server-setup-paths ()
  "Add all installed language servers to the from of the current buffer's `exec-path'."
  (interactive)
  (make-directory my/eglot-server--npm-dir 'parents)
  (setq-local exec-path
	      (append
	       (directory-files-recursively my/eglot-server--npm-dir "bin$" t)
	       (exec-path)))
  (setq-local process-environment
	      (append
	       (list (concat
		      "PATH="
		      (cl-reduce (lambda (a b) (concat a ":" b)) (exec-path))))
	       process-environment))
  )

(defun  my/eglot-server-reset-paths ()
  "Remove any modifications to the current buffer's `exec-path'."
  (interactive)
  (kill-local-variable 'exec-path))

(cl-defun my/eglot-server-create-server (server package-name installer &key extra-dependancies)
  (cl-assert (functionp installer)
	     "INSTALLER must be a function")
  (cl-assert (stringp server)
	     "%s is not a string. SERVER must be the name of a language server" server)
  (cl-assert (stringp package-name)
	     "%s is not a string. package-name must the name of the package required for this server" server)
  (cl-assert (list-of-strings-p extra-dependancies)
	     "%s is not a list of strings.
EXTRA-DEPENDANCIES must be a list of strings with each of which will be installed with INSTALLER"
	     extra-dependancies)
  (record 'my/eglot-server
	  server
	  package-name
	  installer
	  extra-dependancies))

(defvar my/eglot-server-install-dir
  (expand-file-name (locate-user-emacs-file
		     (file-name-concat ".cache" "eglot-servers"))))

(defvar my/eglot-server--npm-dir (file-name-concat my/eglot-server-install-dir "npm"))
(defvar my/eglot-server--cargo-dir (file-name-concat my/eglot-server-install-dir "cargo"))
;; TODO add a way to delete a dependency
;; TODO add a way to check for a dependency
(defun my/eglot-server--npm-package-path (package)
  (file-name-concat my/eglot-server--npm-dir package))

(defun my/eglot-server-server-name (server)
  (aref server 1))

(defvar my/eglot-server-dape-lldb-download-url "https://github.com/vadimcn/codelldb/releases/download/v1.10.0/codelldb-x86_64-linux.vsix")

(defvar my/eglot-server-dape-lldb-path (expand-file-name "~/.emacs.d/debug-adapters"))

(defun my/eglot-server-dape-install-lldb-setup (&optional _package)
  (let ((url my/eglot-server-dape-lldb-download-url)
	(dest my/eglot-server-dape-lldb-path))
    (let ((temp-file (make-temp-file "ext" nil ".zip"))
	  (unzip (executable-find "unzip")))
      (url-copy-file url temp-file 'overwrite)

      (when (file-exists-p dest)
	(delete-directory dest 'recursive))

      (make-directory dest 'parents)
      ;; TODO support windows
      (shell-command (format "%s %s -d %s/codelldb" unzip temp-file dest)))))

(defun my/eglot-server--install (server)
  (cl-assert (eql (type-of server) 'my/eglot-server))
  (let ((package (aref server 2))
	(installer (aref server 3))
	(extra-dependancies (aref server 4)))
    (funcall installer package)
    (cl-loop for p in extra-dependancies
	     do (funcall installer p))))


(defun my/eglot-server--generic-sentinel (package)
  (cl-assert (stringp package) "%s must be a string" package)
  (lambda (process event)
    ;; TODO switch to using `process-status' instead of `event'
    (with-current-buffer (process-buffer process)
      (ansi-color-apply-on-region (point-min) (point-max)))
    (pcase event
      ("finished\n" (message "%s was successfully installed" package))
      ;; ("deleted\n" (message "%s was deleted please retry installing %s" proc-name package))
      ;; ("exited abnormally with code exitcode (core dumped)\n")
      ;; ("failed with code fail-code\n")
      ;; ("signal-description (core dumped)(\n)")
      ("open from host-name\n")
      ("open\n")
      ("run\n")
      ("connection broken by remote peer\n")
      (_ (error "Unhandled case %s for event from %s" event process)))
    ))

;; TODO this really not fully functional yet
(defun my/eglot-server--cargo-dependancy-install (extra-args package)
  "Install the associated cargo package for the given SERVER."
  ;; TODO ensure we exchange string to the proper type of a server later
  (cl-assert (list-of-strings-p extra-args) "%s is not a list of strings. The list should be a list of arguments passed to cargo")
  (cl-assert (stringp package) "%s is not a string corresponding to a registered server")
  (if-let ((cargo (executable-find "cargo"))
	   (proc-name (concat "eglot-installing-" package))
	   (buf-name (upcase (concat "*" proc-name "*"))))
      (progn
	(unless (file-exists-p my/eglot-server--cargo-dir)
	  (make-directory my/eglot-server--cargo-dir 'parents))
	(if (process-live-p (get-buffer-process (get-buffer buf-name)))
	    (message "%s is already being downloaded" package)
	  (make-process :name proc-name
			:buffer buf-name
			:command
			(append (list cargo
				      "install"
				      package
				      "--root"
				      (file-name-concat my/eglot-server--cargo-dir package))
				extra-args)
			:sentinel (my/eglot-server--generic-sentinel package))))
    (error "cargo must be installed in order to unstall %s" package)))

(defun my/eglot-server--npm-dependancy-install (package)
  "Install the associated npm package for the given SERVER."
  ;; TODO ensure we exchange string to the proper type of a server later
  (cl-assert (stringp package) "%s is not a string corresponding to a registered server")
  (if-let ((npm (executable-find "npm"))
	   (proc-name (concat "eglot-installing-" package))
	   (buf-name (upcase (concat "*" proc-name "*"))))
      (progn
	(unless (file-exists-p my/eglot-server--npm-dir)
	  (make-directory my/eglot-server--npm-dir 'parents))
	(if (process-live-p (get-buffer-process (get-buffer buf-name)))
	    (message "%s is already being downloaded" package)
	  (make-process :name proc-name
			:buffer buf-name
			:command
			(list npm
			      "-g"
			      "--prefix"
			      (file-name-concat my/eglot-server--npm-dir package)
			      "install"
			      package)
			:sentinel (my/eglot-server--generic-sentinel package))))
    (error "npm must be installed in order to unstall %s" package)))



(defun my/eglot-server-dependancy (server-name &rest defs)
  (push (cons server-name defs) my/eglot-server--installable-servers))

(defun my/eglot-server-lookup (server-name)
  (alist-get server-name my/eglot-server--installable-servers))

(defun my/eglot-server-reset (server-name)
  (setq my/eglot-server--installable-servers nil))

(defun my/eglot-server-install (server-name)
  (interactive (list (completing-read "Select your server: "(mapcar #'my/eglot-server-server-name my/eglot-server--installable-servers))))
  (my/eglot-server--install (cl-find server-name my/eglot-server--installable-servers
				     :test #'string=
				     :key #'my/eglot-server-server-name)))

(ert-deftest adding-dependancies-and-lookup-test ()
  (my/eglot-server-dependancy 'me 'thing)
  (should (eql (my/eglot-server-lookup 'me) 'thing)))

(ert-deftest install-vue-langauge-server ()
  (let ((final-exec-path "~/.emacs.d/.cache/eglot-servers/npm/@vue/language-server/bin/vue-language-server"))
    (when (file-exists-p final-exec-path)
      (delete-directory "~/.emacs.d/.cache/eglot-servers"))
    (my/eglot-server--npm-dependancy-install "@vue/language-server")
    (should (file-exists-p "~/.emacs.d/.cache/eglot-servers/npm/@vue/language-server/bin/vue-language-server"))))

(defvar my/eglot-server--installable-servers
  (append (list
	   (my/eglot-server-create-server "volar" "@vue/language-server"
					  #'my/eglot-server--npm-dependancy-install
					  :extra-dependancies
					  '("typescript"))
	   (my/eglot-server-create-server "dockerfile-language-server" "dockerfile-language-server-nodejs@0.11.0"
					  #'my/eglot-server--npm-dependancy-install)
	   (my/eglot-server-create-server "vscode-css-language-server" "vscode-langservers-extracted"
					  #'my/eglot-server--npm-dependancy-install)
	   (my/eglot-server-create-server "yaml-language-server" "yaml-language-server"
					  #'my/eglot-server--npm-dependancy-install)
	   (my/eglot-server-create-server "bash-language-server" "bash-language-server"
					  #'my/eglot-server--npm-dependancy-install)
	   (my/eglot-server-create-server "grammarly" "@emacs-grammarly/grammarly-languageserver"
					  #'my/eglot-server--npm-dependancy-install)
	   (my/eglot-server-create-server "purescript-language-server" "purescript-language-server"
					  #'my/eglot-server--npm-dependancy-install
					  :extra-dependancies
					  '("spago" "purescript"))
	   ;; Not a LSP but I hate having to install it seperately
	   (my/eglot-server-create-server "purescript-tidy" "purs-tidy"
					  #'my/eglot-server--npm-dependancy-install)
	   (my/eglot-server-create-server "elm-language-server" "@elm-tooling/elm-language-server"
					  #'my/eglot-server--npm-dependancy-install)
	   ;; TODO determine if more dependencies are needed
	   (my/eglot-server-create-server "elm-language-server" "@elm-tooling/elm-language-server"
					  #'my/eglot-server--npm-dependancy-install)
	   (my/eglot-server-create-server "c++/c/rust debugger" "codelldb"
					  #'my/eglot-server-dape-install-lldb-setup)

	   )
	  ;; vscode-langservers-extracted covers a ton of servers
	  (mapcar
	   (lambda (server-name)
	     (my/eglot-server-create-server server-name "vscode-langservers-extracted"
					    #'my/eglot-server--npm-dependancy-install))
	   (list "vscode-html-language-server"
		 "vscode-css-language-server"
		 "vscode-json-language-server"
		 "vscode-eslint-language-server"))))
