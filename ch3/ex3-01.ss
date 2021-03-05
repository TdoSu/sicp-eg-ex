(load "utils.ss")

;;; 累加器

(define (make-accumulator count)
  (lambda (x)
    (set! count (+ count x))
    count))

(define A (make-accumulator 5))

(display-newline (A 10))
(display-newline (A 10))

(exit)

