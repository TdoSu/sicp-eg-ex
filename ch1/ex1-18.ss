(load "utils.ss")

;;; 用快速幂的思路实现乘法 -- 迭代写法

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (fast-* a b)
  (define (iter p1 p2 result)
    (cond ((= p2 0) result)
          ((even? p2) (iter (double p1) (halve p2) result))
          (else (iter p1 (- p2 1) (+ p1 result)))))
  (iter a b 0))

(display-newline (fast-* 3 5))
(display-newline (fast-* 12 10))

(exit)

