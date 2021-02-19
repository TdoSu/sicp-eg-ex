(load "utils.ss")

;;; 猜测解释器求值结果

(display-newline (list 'a 'b 'c))
;;; > (a b c)

(display-newline (list (list 'george)))
;;; > (('george))

(display-newline (cdr '((x1 x2) (y1 y2))))
;;; > ((y1 y2))

(display-newline (cadr '((x1 x2) (y1 y2))))
;;; > (y1 y2)

(display-newline (pair? (car '(a short list))))
;;; > #f

(display-newline (memq 'red '((red shoes) (blue socks))))
;;; > #f

(display-newline (memq 'red '(red shoes blue socks)))
;;;> (red shoes blue socks)

(exit)

