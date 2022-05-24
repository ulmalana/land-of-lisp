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
## AND and OR
### **Shortcut Boolean Evaluation**:
* OR: **return T immediately**  when the **first element is T**
* AND: **return NIL immediatly** when the **first element is NIL**

## Returning more than the Truth
Some functions return not only the truth value, but also ordinari values.
Example: `(if (member 1 '(3 4 1 5))
                'one-in-list
                'one-not-in-list)`

          -> returns `'ONE-IN-LIST`

          `(member 1 '(3 4 1 5))`

          -> returns `(1 5)`
          **to distinguish between empty list (false) as return value and
          non-empty list (true) as return value**

## Comparing stuff
There are too many ways to compare stuff in Lisp.

### Rule of thumb for comparation
* **Use `EQ` to compare symbols** (`EQ` is simple and fast)
    * example: `(eq 'apple 'apple)`
* **Use `EQUAL` for everything else**
    * example: `(equal (list 1 2 3) (list 1 2 3)`, `(equal 5 5)`, `(equal "bar" "bar")`

### Other comparation functions
* `EQL`: simple like `EQ`, but **can handle numbers and characters**.
    * example: `(eql 4.5 4.5)`, `()`
* `EQUALP`: same as `EQUAL`, but **can handle difficult comparison**.
    * example: `(equalp "riz" "RiZ")` -> `T`, `(equalp 0 0.0)` -> `T`
* The remaining comparison commands are just specialization for specific datatypes.