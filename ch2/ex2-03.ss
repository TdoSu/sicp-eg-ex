(load "utils.ss")

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

;;; 定义矩形

;;; 实现1
;;; 三个顶点
(define (make-rect p1 p2 p3) (cons p1 (p2 p3)))
(define (p1-rect r) (car r))
(define (p2-rect r) (cadr r))
(define (p3-rect r) (cddr r))

;;; 实现2
;;; 两个相邻的边
(define (make-rect s1 s2) (cons s1 s2))
(define (s1-rect r) (car r))
(define (s2-rect r) (cdr r))

(define (length-segment s)
  (distance-points (start-segment s) (end-segment s)))

(define (distance-points p1 p2)
  (let ((x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2)))
    (sqrt (sum-of-squares (- x2 x1) (- y2 y1)))))

(define (sum-of-squares x y) (+ (square x) (square y)))

;;; 求周长

(define (circumference-rect r)
  (let ((s1 (s1-rect r))
        (s2 (s2-rect r)))
    (* 2 (+ (length-segment s1) (length-segment s2)))))

;;; 求面积

(define (area-rect r)
  (let ((s1 (s1-rect r))
        (s2 (s2-rect r)))
    (* (length-segment s1) (length-segment s2))))

(define r (make-rect (make-segment (make-point 0 0) (make-point 5 0))
                     (make-segment (make-point 0 0) (make-point 0 7))))

(display-newline (circumference-rect r))
(display-newline (area-rect r))

(exit)

