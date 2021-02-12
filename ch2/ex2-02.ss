(load "utils.ss")

;;; 实现点和线段

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (begin (display "(")
         (display (x-point p))
         (display ", ")
         (display (y-point p))
         (display ")")))

;;; 实现求线段中点
(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (let ((x1 (x-point start))
          (y1 (y-point start))
          (x2 (x-point end))
          (y2 (y-point end)))
      (make-point (average x1 x2)
                  (average y1 y2)))))

(print-point
  (midpoint-segment
    (make-segment (make-point 0 2)
                  (make-point 4 8))))
(newline)

(exit)

