;;; NOTES
;;;
;;; PRINT starts with a new line before printing. keeps the original data type
;;; PRIN1 brings no new line. (more simple and favorable)

(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))

(defun add-five ()
  (print "Please enter a number:")
  (let ((num (read)))
    (print "When I add five I get: ")
    (print (+ num 5))))

;;; PRINC is more suitable for human read.
(defun say-hello-2 ()
  (princ "Please type  your name: ")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))
