(load "utils.ss")

;;; 可以骗过费马检测的 Carmichael 数
;;; 561, 1105, 1729, 2465, 2821, 6601

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (test a)
    (cond ((>= a n) #t)
          ((try-it a) (test (+ a 1)))
          (else #f)))
  (test 1))

; (display-newline (fermat-test 13))
; (display-newline (fermat-test (* 13 15)))

(display-newline (fermat-test 561))
(display-newline (fermat-test 1105))
(display-newline (fermat-test 1729))
(display-newline (fermat-test 2465))
(display-newline (fermat-test 2821))
(display-newline (fermat-test 6601))

;;; 返回值都是 #t

(exit)

