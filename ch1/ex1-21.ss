(load "utils.ss")

;;; 利用 smallset-divisor 找出 199, 1999, 19999 的最小因子

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(display-newline (smallest-divisor 199))
; > 199
(display-newline (smallest-divisor 1999))
; > 1999
(display-newline (smallest-divisor 19999))
; > 7

(exit)

