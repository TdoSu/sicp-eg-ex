(load "utils.ss")

;;; 改进求平方根程序的 good-enough?

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

;;; 这个过程的问题是:
;;; 如果 x 的值很小, 比如小于 0.0001, 这个判断就会失去效果,
;;; 如果 x 的值很大, 0.0001 的精度要求可能不必要.
;;; 这个精度应该和 x 的值的大小有某种关系.

;;; 另外: 数字特别大的时候, 可能因为精度溢出导致无法完成判断,
;;; 陷入死循环

;;; 改进版本
(define (good-enough? guess x)
  (< (abs (- (/ (square guess) x) 1.0)) 0.0001))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (try guess)
    (if (good-enough? guess x)
        guess
        (try (improve guess))))
  (try 1.0))

;;; 改进版本2
;;; 比较两次迭代 guess 的变化
;;; 有点不动点的味道了

(define (sqrt x)
  (define (try guess)
    (define (improve guess)
      (average guess (/ x guess)))
    ;;; 用 let 避免重复计算 improve guess
    (let ((next (improve guess)))
      (define (good-enough? guess)
        ;;; TODO 定义一个 distance 抽象这个过程
        (< (abs (- (/ next guess) 1.0)) 0.0001))
      (if (good-enough? guess)
          guess
          (try next))))
  (try 1.0))

(display-newline (sqrt (square 0.00001)))
; > 1.0e-5

(display-newline (sqrt (square 90000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)))

(exit)

