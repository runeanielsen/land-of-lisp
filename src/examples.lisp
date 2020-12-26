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
