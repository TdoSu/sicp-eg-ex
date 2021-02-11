(load "utils.ss")

;;; 实现  compose
;;; x -> f(g(x))

(define (compose f g)
  (lambda (x) (f (g x))))

(display-newline ((compose square inc) 6))

(exit)

