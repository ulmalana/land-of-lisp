;;;; Guessing user's number

;;; the computer guess our number and user gives hint to computer
;;; whether the guess should be smaller or bigger.
;;; example: we chose 23.
;;; > (guess-my-number)
;;; 50
;;; > (smaller)
;;; 25
;;; > (smaller)
;;; 12
;;; > (bigger)
;;; 18
;;; > (bigger)
;;; 21
;;; > (bigger)
;;; 23

;;; define global variable for the max number
(defparameter *big* 100)

;;; define global variable for the min number
(defparameter *small* 1)

;;; DEFPARAMETER can overwrite any previously stored value
;;; DEFVAR wont overwrite previous values of global

;;; the computer will guess our number with this function
(defun guess-my-number ()
  ;; ASH will shift the number to the right or to the left
  ;; 1 will shift 1 to the right
  ;; -1 will shift 1 to the left
  (ash (+ *small* *big*) -1)) ; pick the average of small and big as guess

;;; this function will decrease the *big* variable
(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number)) ;; show the current guess

;;; this function will increase the *small* variable
(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number)) ;; show the current guess

;;; reset the global variables to start over a guess
(defun start-over ()
  (defparameter *big* 100)
  (defparameter *small* 1)
  (guess-my-number))


;;;; local variables

;;; this function creates local variable and perform simple arithmetic
;; A and B cant be used outside the LET scope
(let ((a 5)
      (b 6))
  (+ a b)) ;; returns 11

;;;; local functions
;;; with FLET, function F and G cant call each other
(flet ((f (n)
         (+ n 10))
       (g (n)
         (- n 3)))
  (g (f 5))) ;; returns 12

;;; with labels G can call F
(labels ((f (n)
           (+ n 5))
         (g (n)
           (+ (f n) 6)))
  (g 10)) ;; returns 21
