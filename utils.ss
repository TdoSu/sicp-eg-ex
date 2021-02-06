(define (display-newline . msgs)
  (if (null? msgs)
      (newline)
      (begin (display (car msgs))
             (display " ")
             (apply display-newline (cdr msgs)))))

(define (square x) (* x x))

(define (average . numbers)
  (if (null? numbers)
      0
      (/ (sum-of-numbers numbers)
         (length-of-items numbers))))

(define (sum-of-numbers numbers)
  (accumulate numbers + 0))

(define (accumulate items op initial)
  (if (null? items)
      initial
      (op (car items)
          (accumulate (cdr items)
                      op
                      initial))))

(define (length-of-items items)
  (accumulate items
              (lambda (c r) (+ 1 r))
              0))

