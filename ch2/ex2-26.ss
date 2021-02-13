(load "utils.ss")

;;; 猜测解释器打印的结果

(define x (list 1 2 3))
(define y (list 4 5 6))

(display-newline (append x y))
; (1 2 3 4 5 6)
(display-newline (cons x y))
; ((1 2 3) 4 5 6)
(display-newline (list x y))
; ((1 2 3 ) (4 5 6))

(exit)

