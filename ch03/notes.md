# Chapter 03 - Syntax

* Syntax : basic rules to follow to create valid sentence
* Semantics = meaning
    * Examples:
    * My dog ate my homework
    * The canine, which I possess, has consumed my school assignment
    * Der Hund hat mein Hausarbeit gefressen

* Having a simple syntax is a defining feature of Lisp.
* Lisp code is organized into list.
* What can be put into this list:
    * Symbols: letters, numbers, and other chars
    * Numbers: integers, floating points
    * Strings: "like this"

# Code vs Data
which one to be executed? which one just data?
## Code mode
* for instructing something
* a special command is located at the beginning
* example: (expt 2 3) -> 2^3 -> 8
## Data mode
* for keeping any information without executing it
* started with single quote (') or QUOTE command (aka quoting)
* example: '(expt 2 3) -> returns (expt 2 3), without evaluation

## Cons
List in Lisp is build with CONS or LIST.
* example: list with one item named CHICKEN `(cons 'chicken 'nil)` -> `(CHICKEN)`. `NIL` marks the end of list.
* another example: list with three items
    * `(cons 'beef (cons 'chicken (cons 'pork 'nil)))`
    * `(BEEF CHICKEN PORK)`
* simpler version: `(list 'pork 'beef 'chicken)`
### CAR vs CDR
* CAR: get the **first element of list** (like `head` in Haskell)
    * (car '(pork beef chicken)) -> PORK
* CDR: get **all elements excep the head** (like `tail` in Haskell)
    * (cdr '(pork beef chicken)) -> (BEEF CHICKEN)
* Combinations:
    * CADR: equivalent to `head . tail` in Haskell
    * CDAR: equivalent to `tail . head` in Haskell