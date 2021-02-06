(load "utils.ss")

;;; 用牛顿法求立方根

(define (cube-root x)
  (define (good-enough? guess)
    (< (abs (- (/ (cube guess) x) 1.0)) 0.0001))
  (define (improve guess)
    (average guess guess (/ x (square guess))))
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (try 1.0))

(define (cube x) (* x x x))

;;; 如果使用比较 guess 和 improve guess
;;; 那么 good-enough? 都不需要修改
(define (cube-root x)
  (define (good-enough? guess)
    (< (abs (- (/ (improve guess) guess) 1.0)) 0.0001))
  (define (improve guess)
    (average guess guess (/ x (square guess))))
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (try 1.0))

;;; 其实 improve 就是要求不动点的过程
;;; improve 可以通过牛顿法由 cube-root 求出来

(display-newline (cube-root (cube 3.0)))
(display-newline (cube-root (cube 0.0001)))

(exit)

