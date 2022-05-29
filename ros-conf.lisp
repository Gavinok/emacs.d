#|
This is just the config I have roswell load before every repl
|#
(setq *print-case* :downcase)
(declaim (optimize (speed 3) (debug 3) (safety 3)))
(defun my/table (&rest key-and-value &key &allow-other-keys)
  (let ((new-table (make-hash-table)))
    (loop for (sym num) on key-and-value by #'cddr
          do (setf (gethash sym new-table) num))
    new-table))

(defun my/add-to-local-project ()
  (pushnew (uiop:getcwd) ql:*local-project-directories*))
