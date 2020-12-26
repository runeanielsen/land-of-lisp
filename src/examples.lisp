;;; Page 50 - Empty lists equals false
;;; Evaluates to I-AM-FALSE
(if '()
    'i-am-true
    'i-am-false)

;;; Avaluates to I-AM-TRUE
(if '(1)
    'i-am-true
    'i-am-false)

;;; Page 51 - Example of using recursing to get length of list
(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

;;; Page 51 - Any value that is not equivalent to an empty list will
;;; be considered true
(eq '() nil)   ; ==> T
(eq '() ())    ; ==> T
(eq '() 'nil)  ; ==> T

;;; Page 52 - conditionals
(if (= (+ 1 2) 3)
    'yup
    'nop) ; ==> YUP

(if (= (+ 1 2) 4)
    'yup
    'nop) ; ==> NOP

(if '(1)
    'the-list-has-stuff-in-it
    'the-list-is-empty) ; ==> THE-LIST-HAS-STUFF-IN-IT

(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty) ; ==> THE-LIST-IS-EMPTY

(if (oddp 5)
    'odd-number
    'even-number) ; ==> ODD-NUMBER

;;; Page 54 - To do two or more things in an 'if' branch use 'progn'.
(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t)
           'odd-number)
    'even-number) ; => *number-was-odd* will eval to T

;;; Page 55 - 'when' and 'unless' has implicit 'progn'
(defvar *number-is-odd* nil)

(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number) ; => *number-is-odd* will eval to T and returns 'odd-number

(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number) ; => *number-is-odd* will eval to NIL and returns 'even-number
