(load "utils.ss")

;;; 实现 smooth 过程

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (smooth-n n)
  (repeated smooth n))

(exit)

