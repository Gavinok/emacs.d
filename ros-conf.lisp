#|
This is just the config I have roswell load before every repl
|#
(setq *print-case* :downcase)
(declaim (optimize (speed 3) (debug 3) (safety 0)))
(defun my/table (&rest key-and-value &key &allow-other-keys)
  (let ((new-table (make-hash-table)))
    (loop for (sym num) on key-and-value by #'cddr
          do (setf (gethash sym new-table) num))
    new-table))

(defmacro \ (args &body forms)
  `(function (lambda ,args ,@forms)))

(deftype seq-or-fn0 ()
  '(or sequence
    (function () t)))

(declaim (ftype (function ((function (&rest t) t)
                           seq-or-fn0
                           &rest seq-or-fn0)
                          list)
                fmap))
(defun fmap (fn &rest seq-or-fn)
  "A generic way to map over sequences as well as functions

FN is a function with as many arguments as there are elements in
seq-or-fn.

All functions will be applied first then the lists.

for example

(with-open-file (stream #p\"~/res.txt\")
  (fmap (lambda (fn1 fn2) (cons fn1 fn2))
            (lambda () (read-line tmp nil))
            (lambda () (read-line tmp nil))))

Will iterate through all lines of a file 2 at a time 

(with-open-file (stream #p\"~/res.txt\")
  (fmap (lambda (fn1 fn2) (cons fn1 fn2))
            (lambda () (read-line tmp nil))
            (lambda () (read-line tmp nil))
            '(1 2 3)))

Will iterate through all lines of a file 2 at a time as well as the
list. The list will always run to completion.

If only provided functions to use for iterations it will iterate
until a function returns nil"
  (let ((fns (remove-if-not #'functionp seq-or-fn))
        (seqs (remove-if-not (lambda (x) (typep x 'sequence)) seq-or-fn)))
    (cond
      (seqs (apply #'map 'list
                   (lambda (&rest elements)
                     (apply fn (nconc (mapcar #'funcall fns) elements)))
                   seqs))
      (fns (loop for res = (mapcar #'funcall fns)
                 until (some #'null res)
                 collect (apply fn res))))))

(defun my/add-to-local-project ()
  (pushnew (uiop:getcwd) ql:*local-project-directories*))

(declaim (ftype (function ((function (&rest t) t)
                           &rest seq-or-fn0)
                          list)
                map-dwim))
(defun map-dwim (fn &rest seq-or-fn)
  (declare (type (function (&rest t) t) fn))
  (let ((fns (mapcar
              (\ (l)
                 (typecase l
                   (list (let ((nxt nil)
                               (rst l))
                           (\ ()
                              (when rst
                                (setf nxt (car rst)
                                      rst (cdr rst))
                                nxt))))
                   (vector (let ((nxt -1)
                                 (sz (- (length l) 1)))
                             (\ ()
                                (declare (type fixnum nxt))
                                (when (< nxt sz)
                                  (setf nxt (1+ nxt))
                                  (aref l nxt)))))
                   (function l)))
              seq-or-fn)))
    (loop for res = (loop for f in fns
                          as r = (funcall (the function f))
                          if (null r)
                            return nil
                          else
                            collect r)
          until (null res)
          collect (apply fn res))))
