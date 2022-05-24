# Chapter 4 - Conditions

## Empty = False
* NIL = ()
* example
    * `(if '()
            'true
            'false)`
        FALSE
    * `(if '(0)
           'true
           'false)`
        TRUE
## False equivalents
* `'()`
* `()`
* `'nil`
* `nil`
    * `(eq '() nil)`  --> T
    * `(eq '() 'nil)` --> T

## IF Structure
`(if <predicate>
     <true-form>
     <else-form>)`
* Use PROGN function to put > 1 form in each branch
     * example: `()`

## WHEN and UNLESS
* `WHEN`: all the enclosed expressions are evaluated **when the condition is true**
    * example: `(when (oddp 5)
                      (setf *num-is-odd* t)
                      'odd-number)`
* `UNLESS`: all the enclosed expressions are evaluated **when the condition is false**
    * example: `(unless (oddp 4)
                        (setf *num-is-odd* nil)
                        'even-number)`
## One for all: COND
* Use `COND` for multibranch condition
* example: `(defun who-is-this (person)
                   (cond ((eq person 'henry) 'this-is-henry)
                         ((eq person 'johnny) 'this-is-johnny)
                         (t                   'this-is-anyone-else)))`
* the example above will be checked from top down

## CASE Condition
* supply a value for making a comparison
* example: `(defun who-is-this (person)
                   (case person
                         ((henry) 'this-is-henry)
                         ((johnny) 'this-is-johnny)
                         (otherwise 'this-is-stranger))`