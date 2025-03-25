;; Minimal UI
(when  (eq system-type 'android)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		         (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (format "%s:%s"
				    "/data/data/com.termux/files/usr/lib"
				    (getenv "LD_LIBRARY_PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))

(setq-default
 package-native-compile t
 default-frame-alist
 '((tool-bar-lines . 0)
   (menu-bar-lines . 0)
   (undecorated . t)
   (vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil))
 ;; Speedup startup
 file-name-handler-alist nil
 gc-cons-threshold most-positive-fixnum)
