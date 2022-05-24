;;;; Chapter 4 Conditions

;;; this function calculates the length of list recursively
(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

;;; use PROGN to put > 1 form in each branch
(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t)
           'odd-number)
    'even-number)
