(load "utils.ss")

;;; 用序列重构计算零钱的方式

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount coin-values)
  (define (first-denomination coin-values) (car coin-values))
  (define (except-firts-denomination coin-values) (cdr coin-values))
  (define (no-more? coin-values) (null? coin-values))
  (define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+ (cc amount (except-firts-denomination coin-values))
                   (cc (- amount (first-denomination coin-values))
                       coin-values)))))
  (cc amount coin-values))

(display-newline (count-change 100 us-coins))
(display-newline (count-change 100 uk-coins))

(exit)

