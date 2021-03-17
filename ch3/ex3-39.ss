(load "utils.ss")

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
(parallel-execute (lambda () (set! x (* x x)))
                  (s (lambda () (set! x (+ x 1)))))

;;; 改写后 ab 不会被拆开, 剩下 4 种情况
; 1ab23 --> 110
; 12ab3 --> 100
; 123ab --> 101
; ab123 --> 121

(exit)

