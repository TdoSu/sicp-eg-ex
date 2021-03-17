(load "utils.ss")

(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! x (s (lambda () (* x x)))))
                  (s (lambda () (set! x (+ x 1)))))

; 1. (set! m1 x)
; 2. (set! m2 x)
; 3. (set! x (* m1 m2))

; a. (set! a x)
; b. (set! x (+ a 1))

; 串行化之后 ab 和 12 不能交替进行

; 1ab23 --> 110 x
; 12ab3 --> 100 v
; 123ab --> 101 v
; ab123 --> 121 v
; a123b --> 11  x

;;; 还有三种情况

(exit)

