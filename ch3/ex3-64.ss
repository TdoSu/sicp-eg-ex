(load "utils.ss")

;;; 尝试实现流

(define (cons-stream start rest) (cons start rest))

(define (stream-car s) (car s))

(define (stream-cdr s) ((cdr s)))

(define (integers-starting-from n)
  (cons-stream n (lambda () (integers-starting-from (+ n 1)))))

(define integers (integers-starting-from 1))

(define (stream-null? s) (null? s))

(define the-empty-stream '())

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (lambda () (stream-map proc (stream-cdr s))))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s) (lambda () (stream-filter pred (stream-cdr s)))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;;; 用流的方式描述平方根过程

(define (sqrt-stream x)
  (define (sqrt-improve guess)
    (average guess (/ x guess)))
  (define guesses
    (cons-stream 1.0
                 (lambda ()
                   (stream-map (lambda (guess)(sqrt-improve guess))
                               guesses))))
  guesses)

(display-newline (stream-ref (sqrt-stream (square 3)) 10))

(define (good-enough? x)
  (lambda (guess)
    (< (abs (- x (square guess))) 0.0001)))

(display-newline (stream-ref (stream-filter (good-enough? 2)
                                            (sqrt-stream 2))
                             0))

(display-newline "------")

(define (stream-limit s tolerance)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (if (close-enough? (stream-car s) (stream-car (stream-cdr s)))
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(display-newline (sqrt 2 0.0001))

(exit)

