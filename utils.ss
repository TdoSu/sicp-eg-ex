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

;;; (real-time) the amount of real time that has elapsed since system start-up
;;; system -- chez scheme
;;; 单位是 1/1000 second
(define (runtime) (real-time))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (and (> n 1) (= (smallest-divisor n) n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  ; (random x) --> a nonnegative pseudo-random number less than x
  ; x 是整数得到的就是整数, x 是小数得到的就是小数
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (identity x) x)

