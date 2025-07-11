;;; mason.el --- Install all the programming tools easy  -*- lexical-binding: t -*-
;;; Commentary:
;; Inspired by mason.nvim
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

;; TODO test the following install options
;; - cargo
;; - pip
;; - golang

;; TODO Support more install types
;; - [ ] build
;; - [X] cargo
;; - [ ] composer
;; - [ ] gem
;; - [X] golang
;; - [ ] luarocks
;; - [X] npm
;; - [ ] nuget
;; - [ ] opam
;; - [ ] openvsx
;; - [X] pip

;;; Code:

(require 'ansi-color)
(require 'cl-lib)

(defgroup mason nil
  "Install language servers, debug adapters, linters, and other programming tools for you."
  :group 'tools)

(defvar mason-install-dir
  (expand-file-name (locate-user-emacs-file
		     (file-name-concat ".cache" "mason-servers"))))


(defvar mason--npm-dir (file-name-concat mason-install-dir "npm"))
(defvar mason--cargo-dir (file-name-concat mason-install-dir "cargo"))
(defvar mason--pip-dir (file-name-concat mason-install-dir "pip"))
(defvar mason--go-dir (file-name-concat mason-install-dir "golang"))

(defun mason-setup-paths ()
  "Add all installed language servers to the from of the buffer local variable `exec-path'."
  (interactive)
  (setq-local exec-path
	      (flatten-tree
               (list (mapcar (lambda (d)
                               (make-directory d 'parents)
                               (directory-files-recursively d "bin$" t))
                             (list mason--npm-dir
                                   mason--pip-dir
                                   mason--cargo-dir
                                   mason--go-dir))
                     (exec-path))))
  (setq-local process-environment
	      (append
	       (list (concat
		      "PATH="
		      (cl-reduce (lambda (a b) (concat a ":" b)) (exec-path))))
	       process-environment))
  )

(defun mason-reset-paths ()
  "Remove any modifications to the current buffer's local variable `exec-path'."
  (interactive)
  (kill-local-variable 'process-environment)
  (kill-local-variable 'exec-path))

(cl-defun mason-create-server (server package-name installer &key extra-dependancies)
  "Create a new Mason server configuration.

SERVER is the name of the language server as a string.
PACKAGE-NAME is the name of the required package for this server as a string.
INSTALLER is a function used to install the server.
EXTRA-DEPENDANCIES, if provided, is a list of strings representing
additional packages to be installed with the installer."

  (cl-assert (functionp installer)
             "INSTALLER must be a function")
  (cl-assert (stringp server)
             "%s is not a string. SERVER must be the name of a language server" server)
  (cl-assert (stringp package-name)
             "%s is not a string. PACKAGE-NAME must be the name of the package required for this server" server)
  (cl-assert (list-of-strings-p extra-dependancies)
             "%s is not a list of strings.
EXTRA-DEPENDANCIES must be a list of strings with each of which will be installed with INSTALLER"
             extra-dependancies)
  (record 'mason
          server
          package-name
          installer
          extra-dependancies))


(defvar mason-dape-lldb-download-url "https://github.com/vadimcn/codelldb/releases/download/v1.10.0/codelldb-x86_64-linux.vsix")

(defvar mason-dape-lldb-path (expand-file-name "~/.emacs.d/debug-adapters"))

(defun mason-dape-install-lldb-setup (&optional _package)
  "Download and install the LLDB setup for Mason Dape.

This function fetches the LLDB package specified by
`mason-dape-lldb-download-url` and unzips it into
`mason-dape-lldb-path`.  Any existing installation at the destination
will be removed before the new package is installed.

Currently, Windows support is not implemented."
  (let ((url mason-dape-lldb-download-url)
        (dest mason-dape-lldb-path))
    (let ((temp-file (make-temp-file "ext" nil ".zip"))
          (unzip (executable-find "unzip")))
      (url-copy-file url temp-file 'overwrite)

      (when (file-exists-p dest)
        (delete-directory dest 'recursive))

      (make-directory dest 'parents)
      ;; TODO support windows
      (shell-command (format "%s %s -d %s/codelldb" unzip temp-file dest)))))

(defun mason--generic-sentinel (package)
  "Generic sentinel used for handling the instillation of packages.

PACKAGE should be a string containing the name of the package"
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
(defun mason--cargo-dependancy-install (extra-args package)
  "Install the associated cargo package for the given SERVER."
  ;; TODO ensure we exchange string to the proper type of a server later
  (cl-assert (list-of-strings-p extra-args) "%s is not a list of strings. The list should be a list of arguments passed to cargo")
  (cl-assert (stringp package) "%s is not a string corresponding to a registered server")
  (if-let ((cargo (executable-find "cargo"))
	   (proc-name (concat "mason-installing-" package))
	   (buf-name (upcase (concat "*" proc-name "*"))))
      (progn
	(unless (file-exists-p mason--cargo-dir)
	  (make-directory mason--cargo-dir 'parents))
	(if (process-live-p (get-buffer-process (get-buffer buf-name)))
	    (message "%s is already being downloaded" package)
	  (make-process :name proc-name
			:buffer buf-name
			:command
			(append (list cargo
				      "install"
				      package
				      "--root"
				      (file-name-concat mason--cargo-dir package))
				extra-args)
			:sentinel (mason--generic-sentinel package))))
    (error "cargo must be installed in order to unstall %s" package)))

(defun mason--npm-dependancy-install (package)
  "Install the associated npm package for the given SERVER.

PACKAGE is expected to be a string containing the package being installed."
  ;; TODO ensure we exchange string to the proper type of a server later
  (cl-assert (stringp package) "%s is not a string corresponding to a registered server")
  (if-let ((npm (executable-find "npm"))
	   (proc-name (concat "mason-installing-" package))
	   (buf-name (upcase (concat "*" proc-name "*"))))
      (progn
	(unless (file-exists-p mason--npm-dir)
	  (make-directory mason--npm-dir 'parents))
	(if (process-live-p (get-buffer-process (get-buffer buf-name)))
	    (message "%s is already being downloaded" package)
	  (make-process :name proc-name
			:buffer buf-name
			:command
			(list npm
			      "-g"
			      "--prefix"
			      (file-name-concat mason--npm-dir package)
			      "install"
			      package)
			:sentinel (mason--generic-sentinel package))))
    (error "npm must be installed in order to unstall %s" package)))

(defun mason--pip-dependancy-install (package)
  "Install the associated pip package in a virtual environment.

PACKAGE is expected to be a string containing the package being installed."
  (cl-assert (stringp package) "%s is not a string corresponding to a registered server")
  (if-let ((python (executable-find "python"))
           (proc-name (concat "mason-installing-" package))
           (buf-name (upcase (concat "*" proc-name "*"))))
      (progn
        (unless (file-exists-p mason--pip-dir)
          (make-directory mason--pip-dir 'parents))
        (if (process-live-p (get-buffer-process (get-buffer buf-name)))
            (message "%s is already being downloaded" package)
          (let ((venv-dir (file-name-concat mason--pip-dir (concat package "-venv"))))
            (if (file-exists-p venv-dir)
                ;; Virtual environment already exists, just install the package
                (mason--pip-install-package package venv-dir proc-name buf-name)
              ;; Create virtual environment first
              (make-process :name proc-name
                            :buffer buf-name
                            :command (list python "-m" "venv" venv-dir)
                            :sentinel (lambda (_process event)
                                        (if (string-match "finished" event)
                                            (progn
                                              (message "Virtual environment created for %s, installing package..." package)
                                              (mason--pip-install-package package venv-dir proc-name buf-name))
                                          (message "Failed to create virtual environment for %s" package))))))))
    (error "python must be installed in order to install %s" package)))

(defun mason--pip-install-package (package venv-dir proc-name buf-name)
  "Install PACKAGE in the virtual environment at VENV-DIR.

PROC-NAME is the base used for the name of the process being created for
this install.

BUF-NAME is the name of the buffer associated with this process"
  (let ((pip-exe (file-name-concat venv-dir "bin" "pip")))
    (make-process :name (concat proc-name "-install")
                  :buffer buf-name
                  :command (list pip-exe "install" package)
                  :sentinel (mason--generic-sentinel package))))

;; Alternative version using go modules (more modern approach)
(defun mason--go-mod-dependancy-install (package)
  "Install the associated go package using go modules.

PACKAGE is expected to be a string containing the package being installed."
  (cl-assert (stringp package) "%s is not a string corresponding to a registered server")
  (if-let ((go (executable-find "go"))
           (proc-name (concat "mason-installing-" package))
           (buf-name (upcase (concat "*" proc-name "*"))))
      (progn
        (unless (file-exists-p mason--go-dir)
          (make-directory mason--go-dir 'parents))
        (if (process-live-p (get-buffer-process (get-buffer buf-name)))
            (message "%s is already being downloaded" package)
          (let* ((package-name (file-name-base package))
                 (install-dir (file-name-concat mason--go-dir package-name))
                 (bin-dir (file-name-concat install-dir "bin")))
            (unless (file-exists-p bin-dir)
              (make-directory bin-dir 'parents))
            (let ((process-environment
                   (append (list (concat "GOBIN=" bin-dir))
                           process-environment)))
              (make-process :name proc-name
                            :buffer buf-name
                            :command (list go "install" (concat package "@latest"))
                            :sentinel (mason--generic-sentinel package))))))
    (error "go must be installed in order to install %s" package)))

(ert-deftest install-vue-langauge-server ()
  (let ((final-exec-path "~/.emacs.d/.cache/mason-servers/npm/@vue/language-server/bin/vue-language-server"))
    (when (file-exists-p final-exec-path)
      (delete-directory "~/.emacs.d/.cache/mason-servers"))
    (mason--npm-dependancy-install "@vue/language-server")
    (should (file-exists-p "~/.emacs.d/.cache/mason-servers/npm/@vue/language-server/bin/vue-language-server"))))

(defvar mason--installable-servers
  (append (list
	   (mason-create-server "volar" "@vue/language-server"
                                #'mason--npm-dependancy-install
                                :extra-dependancies
                                '("typescript"))
	   (mason-create-server "dockerfile-language-server" "dockerfile-language-server-nodejs@0.11.0"
                                #'mason--npm-dependancy-install)
	   (mason-create-server "vscode-css-language-server" "vscode-langservers-extracted"
                                #'mason--npm-dependancy-install)
	   (mason-create-server "yaml-language-server" "yaml-language-server"
                                #'mason--npm-dependancy-install)
	   (mason-create-server "bash-language-server" "bash-language-server"
                                #'mason--npm-dependancy-install)
	   (mason-create-server "grammarly" "@emacs-grammarly/grammarly-languageserver"
                                #'mason--npm-dependancy-install)
	   (mason-create-server "purescript-language-server" "purescript-language-server"
                                #'mason--npm-dependancy-install
                                :extra-dependancies
                                '("spago" "purescript"))
	   ;; Not a LSP but I hate having to install it seperately
	   (mason-create-server "purescript-tidy" "purs-tidy"
                                #'mason--npm-dependancy-install)
	   (mason-create-server "elm-language-server" "@elm-tooling/elm-language-server"
                                #'mason--npm-dependancy-install)
	   ;; TODO determine if more dependencies are needed
	   (mason-create-server "elm-language-server" "@elm-tooling/elm-language-server"
                                #'mason--npm-dependancy-install)
	   (mason-create-server "c++/c/rust debugger" "codelldb"
                                #'mason-dape-install-lldb-setup)
           (mason-create-server "python-lsp-server" "python-lsp-server[all]"
                                #'mason--pip-dependancy-install)
           (mason-create-server "jedi-language-server" "jedi-language-server"
                                #'mason--pip-dependancy-install)
           ;; ty is not included by default
           ;; (add-to-list  'eglot-server-programs '((python-mode python-ts-mode)  "ty" "server"))
           (mason-create-server "python(ty)" "ty"
                                #'mason--pip-dependancy-install)
           (mason-create-server "gopls" "golang.org/x/tools/gopls"
                                #'mason--go-mod-dependancy-install)
           )
	  ;; vscode-langservers-extracted covers a ton of servers
	  (mapcar
	   (lambda (server-name)
	     (mason-create-server server-name "vscode-langservers-extracted"
                                  #'mason--npm-dependancy-install))
	   (list "vscode-html-language-server"
		 "vscode-css-language-server"
		 "vscode-json-language-server"
		 "vscode-eslint-language-server"))))

(defun mason-server-name (server)
  "Get the name of a server from a mason record named SERVER."
  (aref server 1))

(defun mason--install (server)
  "Helper for installing servers and working with a mason record SERVER."
  (cl-assert (eql (type-of server) 'mason))
  (let ((package (aref server 2))
	(installer (aref server 3))
	(extra-dependancies (aref server 4)))
    (funcall installer package)
    (cl-loop for p in extra-dependancies
	     do (funcall installer p))))

(defun mason-install (server-name)
  "Install SERVER-NAME on the current system."
  (interactive (list (completing-read "Select your server: "(mapcar #'mason-server-name mason--installable-servers))))
  (mason--install (cl-find server-name mason--installable-servers
                           :test #'string=
                           :key #'mason-server-name)))
(provide 'mason)
;;; mason.el ends here

