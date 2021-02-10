(load "utils.ss")

;;; 迭代实现 sum

(define (sum term a next b)
  (define (iter current result)
    (if (> current b)
        result
        (iter (next current) (+ (term current) result))))
  (iter a 0))

(define (sum-integers a b)
  (sum (lambda (x) x) a (lambda (x) (+ x 1)) b))

(define (sum-cubes a b)
  (sum cube a (lambda (x) (+ x 1)) b))

(define (pi-sum a b)
  (sum (lambda (a) (/ 1.0 (* a (+ a 2))))
       a
       (lambda (x) (+ x 4))
       b))

(display-newline (sum-integers 1 100))
(display-newline (sum-cubes 1 3))
(display-newline (* 8 (pi-sum 1 10000)))

(exit)

