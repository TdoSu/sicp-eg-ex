(load "utils.ss")

;;; 实现 repeated

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(display-newline ((repeated square 2) 5))

(exit)

