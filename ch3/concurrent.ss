(load "utils.ss")

(display-newline "------ 并发 ----------")

; (parallel-execute <p1> <p2> ... <pi>)
; 每个 p 是一个没有参数的过程,
; 执行 parallel-execute 时, 每个 p 一个线程并发进行

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

;;; x 有五种可能
; 1. (set! m1 x)
; 2. (set! m2 x)
; 3. (set! x (* m1 m2))
; a. (set! a x)
; b. (set! x (+ a 1))
; 1ab23 --> 110
; 12ab3 --> 100
; 123ab --> 101
; ab123 --> 121
; a123b --> 11

; make-serializer 构造串行化组

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))

; s 修改过的所有 p 不会交错进行
; 结果只有 123ab ab123 两种

(exit)

