(load "util.ss")

(define (make-interval-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (average (lower-bound i) (upper-bound i)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;; 中心偏移量百分比定义的区间

(define (make-center-percent c p)
  (make-interval c (* c p)))

(define (percent i)
  (/ (width i) (center i)))

(exit)

