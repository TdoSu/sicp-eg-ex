(load "utils.ss")

(display-newline "---- STREAM ------")

; (cons-stream x y)
; (stream-car s)
; (stream-cdr s)
; the-empty-stream
; (stream-null? s)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-newline s))

;;; (force (delay expr)) --> ((lambda () (expr)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; (define p
;   (stream-car
;     (stream-cdr
;       (stream-filter prime?
;                      (stream-enumerate-interval 10000 100000000)))))

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda (not already-run?)
      (if (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;;; (delay expr) --> (memo-proc (lambda () expr))


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

(display-newline (stream-ref (stream-filter even? (stream-map square integers)) 0))
(display-newline (stream-ref (stream-filter even? (stream-map square integers)) 1))
(display-newline (stream-ref (stream-filter even? (stream-map square integers)) 2))
(display-newline (stream-ref (stream-filter even? (stream-map square integers)) 3))
(display-newline (stream-ref (stream-filter even? (stream-map square integers)) 4))
(display-newline (stream-ref (stream-filter even? (stream-map square integers)) 5))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(display-newline (stream-ref no-sevens 100))

(define (fibgen a b)
  (cons-stream a (lambda () (fibgen b (+ a b)))))

(define fibs (fibgen 0 1))

(display-newline (stream-ref fibs 0))
(display-newline (stream-ref fibs 1))
(display-newline (stream-ref fibs 2))
(display-newline (stream-ref fibs 3))
(display-newline (stream-ref fibs 4))
(display-newline (stream-ref fibs 5))

;;; 用无穷流-筛法求素数

(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (lambda ()
      (sieve (stream-filter
               (lambda (x)
                 (not (divisible? x (stream-car stream))))
               (stream-cdr stream))))))

(define primes (sieve (integers-starting-from 2)))

(display-newline (stream-ref primes 50))

;;; 另一种实现无穷流的方式

(define ones (cons-stream 1 (lambda () ones)))

;;; ex3-50 扩展的 stream-map
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (lambda ()
                     (apply stream-map
                            (cons proc (map stream-cdr argstreams)))))))

(define (add-stream s1 s2) (stream-map + s1 s2))

(define integers (cons-stream 1 (lambda () (add-stream ones integers))))

(display-newline (stream-ref ones 11))
(display-newline (stream-ref integers 11))

(define fibs
  (cons-stream 0
               (lambda ()
                 (cons-stream 1
                              (lambda ()
                                (add-stream (stream-cdr fibs) fibs))))))

(display-newline (stream-ref fibs 5))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (lambda () (scale-stream double 2))))

(display-newline (stream-ref double 4))

(exit)

