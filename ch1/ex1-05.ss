(load "utils.ss")

; 测试解释器是采用应用序求值还是正则序求值

(define (p) (p))
; p --> (lambda () (p))
; 调用 p 过程, 就会陷入反复调用 p 过程的死循环

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; 解释器采用应用序求值就会陷入死循环,
; 因为会先对 (p) 求值, 就会一致调用过程 p,
; 同时这又是一个尾递归, 所以不会栈溢出

; 如果采用正则序求值, 就不会对 (p) 求值, 会返回 0

;;; 也可以用其他异常值来测试, 比如除以 0

(define (p2) (/ 1 0))

(test 0 (p2))

; 应用序求值会抛出错误
; chez scheme 抛出的是
; Exception in /: undefined for 0

(exit)

