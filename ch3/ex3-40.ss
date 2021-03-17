(load "utils.ss")

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

; 1. (set! m1 x)
; 2. (set! m2 x)
; 3. (set! x (* m1 m2))
; a. (set! n1 x)
; b. (set! n2 x)
; c. (set! n3 x)
; d. (set! x (* n1 n2 n3))

; 1abcd23 --> 10000
; 12abcd3 --> 100
; 123abcd --> 1000000
; a123bcd --> 100000
; ab123cd --> 10000
; abc123d --> 1000
; abcd123 --> 1000000
;; 可能是 100, 1000, 10000, 100000, 1000000

;;; 串行化改写

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

; 123abcd --> 1000000
; abcd123 --> 1000000
;;; 只有一种可能 1000000

(exit)

