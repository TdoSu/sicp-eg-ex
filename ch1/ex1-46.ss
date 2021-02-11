(load "utils.ss")

;;; 抽象一般的迭代式改进过程

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  try)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(display-newline (sqrt (square 3)))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (good-enough? guess)
    (let ((next (f guess)))
      (close-enough? guess next)))
  (define improve f)
  ((iterative-improve good-enough? improve) first-guess))

(display-newline (fixed-point cos 1.0))

(exit)

