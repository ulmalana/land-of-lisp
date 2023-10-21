;;; create alist for each location with its description
(defparameter *nodes* '((living-room (you are in the living-room.
                                      a wizard is snoring loudly in the couch.))
                        (garden (you are in a beautiful garden.
                                 there is a well in front of you.))
                        (attic (you are in the attic.
                                there is a giant welding torch in the corner.))))

;;; create alist of paths to certain location
(defparameter *edges* '((living-room (garden west door)
                         (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

;;; this function gets the description of a given location
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;;; this function gets the description of a path
;;; (`) = quasiquoting : create data with embedded lisp code
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;; describes multipaths
;;; steps:
;;; 1. find the relevant edges
;;; 2. convert the edges to description
;;; 3. join the descriptions
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;; create a list of objects
(defparameter *objects* '(whiskey bucket frog chain))

;;; list of objetcs' location
(defparameter *object-locations* '((whiskey living-room)
                                  (bucket living-room)
                                  (chain garden)
                                  (frog garden)))

;;; this function lists the objects that are visible from a given location
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;;; this function describes the visible objects
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;;; set the initial location to living room
(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

(defun pickup (object)
  (cond
    ((member object (objects-at *location* *objects* *object-locations*))
     (push (list object 'body) *object-locations*)
     `(you are now carrying the ,object))
    (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;; Chapter 06
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond
        ((eq item #\space) (cons item (tweak-text rest caps lit)))
        ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
        ((eq item #\") (tweak-text rest caps (not lit)))
        (lit (cons item (tweak-text rest nil lit)))
        ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
        (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ
   (coerce
    (tweak-text (coerce (string-trim "() "
                                     (prin1-to-string lst))
                        'list)
                t nil)
    'string))
  (fresh-line))
