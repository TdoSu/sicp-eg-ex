(load "utils.ss")

;;; 利用牛顿法求三次方程的根

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(define x (newton-method (cubic 1 2 3) 1.0))
(display-newline ((cubic 1 2 3) x))
;;; 4.935607478273596e-12 ~= 0

(exit)

