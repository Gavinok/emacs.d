;;; add-numbers.el --- Simple addition function

;;; Commentary:
;; This file contains a function to add two numbers together.

;;; Code:

(defun add-numbers (&rest numbers)
  "Add all NUMBERS and return the accumulated result."
  (interactive)
  (let ((result (apply #'+ numbers)))
    (message "Result: %s" result)
    result))

(provide 'add-numbers)

;;; add-numbers.el ends here
