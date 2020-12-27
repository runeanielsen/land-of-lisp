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

;;; Page 56 - Using cond
(defvar *arch-enemy* nil)
(defun pudding-eater-cond (person)
  (cond ((eq person 'hentry) (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny)
         '(i hope you choke on my pudding johnny))
        (t '(why you you eat my pudding stranger?))))

;;; Page 57 - Using case
(defun pudding-eater (person)
  (case person
    ((henry) (setf *arch-enemy* 'stupid-lisp-alien)
     '(curse you lisp alien - you ate my pudding))
    ((johnny) (setf *arch-enemy* 'useless-old-johnny)
     '(i hope you choke on my pudding johnny))
    (otherwise '(why you ate my pudding stranger?))))

;;; Page 58 - Stealth conditionals 'and' 'or'
(and (oddp 5) (oddp 7) (oddp 9)) ; ==> T
(or (oddp 4) (oddp 7) (oddp 8)) ; ==> T

;;; Example of using conditionals to set a global variable to true.
;;; This examples showcases that Lisp uses shortcut Boolean evaluation.
(defparameter *is-it-even* nil)
(or (oddp 4) (setf *is-it-even* t))
(*is-it-even*) ; ==> T

(setf *is-it-even* nil)
(or (oddp 5) (setf *is-it-even* t))
(*is-it-even*) ; ==> NIL

;;; Return more than the Truth
(if (member 1 '(3 4 1 5))
    'one-is-in-the-list
    'one-is-not-in-the-list) ; ==> ONE-IS-IN-THE-LIST

(if (member nil '(3 nil 1 5))
    'nil-is-in-the-list
    'nil-is-not-in-the-list) ; ==> NIL-IS-IN-THE-LIST

;;; Page 61 - 'find-if'
(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-no-odd-number) ; ==> THERE-IS-AN-ODD-NUMBER

;;; Page 63 - eq, equal, and more
;;; 1. Use eq to compare symbols
;;; 2. Use equal for everything else

(defparameter *fruit* 'apple)

(cond ((eq *fruit* 'apple) 'its-an-apple)
      ((eq *fruit* 'orange)) 'its-an-orange) ; ==> ITS-AN-APPLE

;;; Equal can check if two things are isomorphic - meaning look the same

;;; Comparing symbols
(equal 'apple 'apple) ;; ==> T

;;; Comparing lists
(equal (list 1 2 3) (list 1 2 3)) ; ==> T

;;; Equal lists created in different ways still compare as the same
(equal '(1 2 3) (cons 1 (cons 2 (cons 3 ())))) ; ==> T

;;; Comparing integers
(equal 5 5) ; ==> T

;;; Comparing floating point numbers
(equal 2.5 2.5) ; ==> T

;;; Comparing strings
(equal "foo" "foo")

;;; Comparing characters
(equal #\a #\a)

;;; Page 65 - eql is similar to eq command, but unlike eq, it also handles comparisons
;;; of numbers and characters

;;; Comparing symbol
(eql 'foo 'foo) ; ==> T

;;; Comparing numbers
(eql 3.4 3.4) ; ==> T

;;; Comparing characters
(eql #\a #\a)

;;; Page 65 - equalp is esentially the same as equal, except
;;; that it can compare strings with different capitalizations
;;; that is can compare integers against floating-point numbers

;;; Comparing strings with different CAPS
(equalp "Bob Smith" "bob smith") ; ==> T

;;; Comparing integers against floating point numbers
(equalp 0 0.0) ; ==> T

;;; Page 87 - print and read
(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))

(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "When I add five I get")
    (print (+ num 5))))

;;; Note that we put an explicit quote on the front of each value.
;;; It could be omitted in all cases but the symbol, since
;;; a symbol can also refer to functions.
(print '3) ; an integer
(print '3.4) ; a float
(print 'foo) ; a symbol
(print '"foo") ; a string
(print '#\a) ; a character

(progn (princ "This sentence will be interrupted")
       (princ #\newline)
       (princ "by an annoying newline character."))

(defun say-hello-v2 ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))
