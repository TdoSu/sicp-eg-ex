(load "utils.ss")

;;; 利用无穷连分式求正切

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (i)
               (+ 1 (* 2 (- i 1))))
             k))

(display-newline (tan-cf (/ 3.1415926 4) 1000))

(exit)

