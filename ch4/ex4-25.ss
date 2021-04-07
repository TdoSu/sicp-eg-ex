(load "utils.ss")

(define (unless condtion usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

;;; 会陷入死循环 --> 会一直对参数求值, 一直求 factorial
(display-newline (factorial 5))

(exit)

