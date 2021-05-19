(define (display-newline . messages)
  (if (null? messages)
      (newline)
      (begin (display (car messages))
             (display " ")
             (apply display-newline (cdr messages)))))

(define (most-of-numbers p numbers)
  (cond ((null? numbers) (error 'MOST-OF-NUMBERS "没有参数" numbers))
        ((null? (cdr numbers)) (car numbers))
        ((p (car numbers) (most-of-numbers p (cdr numbers))) (car numbers))
        (else (most-of-numbers p (cdr numbers)))))

(define (max-of-numbers . numbers) (most-of-numbers > numbers))

(define (min-of-numbers . numbers) (most-of-numbers < numbers))

(define (filter pred items)
  (cond ((null? items) '())
        ((pred (car items)) (cons (car items) (filter pred (cdr items))))
        (else (filter pred (cdr items)))))

(define (append items1 items2)
  (cond ((null? items1) items2)
        (else (cons (car items1)
                    (append (cdr items1) items2)))))

(define (reverse items)
  (if (null? items)
      '()
      (append (reverse (cdr items))
              (list (car items)))))

(define (sort numbers)
  (if (null? numbers)
      '()
      (let ((privot (car numbers)))
        (let ((rest-numbers (cdr numbers)))
          (append (sort (filter (lambda (x) (<= x privot))
                                rest-numbers))
                  (cons privot
                        (sort (filter (lambda (x) (> x privot))
                                      rest-numbers))))))))

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

(define (square x) (* x x))

(define (sqrt x)
  (define (improve g)
    (average g (/ x g)))
  (define (good-enough? g)
    (let ((next (improve g)))
      (< (abs (- (/ g next) 1)) 0.001)))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

(define (fib n)
  (define (iter a b counter)
    (if (= counter n)
        a
        (iter b (+ a b) (+ counter 1))))
  (iter 0 1 0))

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc (- amount (first-denomination kinds-of-coins))
                       kinds-of-coins)
                   (cc amount
                       (- kinds-of-coins 1))))))
  (cc amount 5))

;;; ----

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

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cont-frac n d k)
  (define (iter counter result)
    (if (= counter 1)
        result
        (iter (- counter 1)
              (/ (n (- counter 1))
                 (+ (d (- counter 1)) result)))))
  (iter k (/ (n k) (d k))))

(define (newton-method g guess)
  (define (newton-transform g)
    (define (deriv g)
      (let ((dx 0.00001))
        (lambda (x)
          (/ (- (g (+ x dx)) (g x))
             dx))))
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq) (accumulate append '() (map proc seq)))

