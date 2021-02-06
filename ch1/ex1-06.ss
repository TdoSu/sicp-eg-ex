(load "utils.ss")

;;; 为什么不能用 cond 实现一个过程代替 if

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; new-if 是一个普通的过程, 解释器采用应用序求值
; 在带入 body 前, 会先对 predicate then-clause else-clause 求值,
; 这和 if 是不同的, if 会先对 predicate 求值, 然后根据结果对 then, else 求值.

; new-if 会在两种情况下出问题:

; 1. then-clause, else-clause 有一个值可能是非法值, 要通过 predicate 避免对它计算.

(define x 0)

(if (= x 0) x (/ 1 x))
; > 0 没有问题

; (new-if (= x 0) x (/ 1 x))
; 会报错  Exception in /: undefined for 0

; 2. 进行递归时一个分支是终止条件, 一个分支是递归调用.
;   因为带入过程前就会反复对两个分支进行计算, 并且不会根据条件有选择的求值,
;   将导致递归调用的分支被没有终止的调用求值, 导致死循环.
;   2.1 两个分支带入前就会被计算
;   2.2 不论 predicate 是 #t #f 两个分支都会被计算
;   --> 递归调用不会被终止.

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (try guess)
    ;;; 这里使用 new-if
    ;;; 会导致程序陷入死循环
    (new-if (good-enough? guess)
            guess
            ;;; 这里是尾递归, 所以不会栈溢出
            (try (improve guess))))
  (try 1.0))

(display-newline (sqrt (square 3)))

(exit)

