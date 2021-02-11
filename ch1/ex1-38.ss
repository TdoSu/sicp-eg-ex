(load "utils.ss")

;;; 利用连分式求 e

(define e
  (+ (cont-frac (lambda (i) 1.0)
                (lambda (i)
                 (cond ((= (remainder i 3) 2)
                        (* (/ (+ i 1) 3) 2))
                       (else 1)))
                10000)
     2))

(display-newline e)

(exit)

