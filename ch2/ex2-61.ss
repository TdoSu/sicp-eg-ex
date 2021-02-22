(load "utils.ss")

(define (adjoin-set x set)
  (cond ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

(display-newline (adjoin-set 5 '(1 2 3 7 8)))

(exit)

