(defun is-even (number)
  (equal (mod number 2) 0))

(defun is-odd (number)
  (> (mod number 2) 0))
