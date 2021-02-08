(load "utils.ss")

;;; 用快速幂的思路实现乘法 -- 递归写法

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (fast-* a b)
  (cond ((= a 0) 0)
        ((even? a) (double (fast-* (halve a) b)))
        (else (+ b (fast-* (- a 1) b)))))

(display-newline (fast-* 3 5))
(display-newline (fast-* 12 10))

(exit)

