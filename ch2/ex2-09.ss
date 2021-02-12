(load "utils.ss")

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upp-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (width-interval i) (/ (- (upper-bound i) (lower-bound i)) 2))

; (width-interval (add-interval a b))
; (width-interval
;   (make-interval (+ (lower-bound x) (lower-bound y))
;                  (+ (upper-bound x) (upper-bound y))))
; (/ (- (+ (upper-bound x) (upper-bound y))
;       (+ (lower-bound x) (lower-bound y)))
;    2)
; ;;; 加法结合律和交换律
; (/ (+ (- (upper-bound x) (lower-bound x))
;       (- (upper-bound y) (lower-bound y)))
;    2)
; ;;; 乘法分配率
; (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;    (/ (- (upper-bound y) (lower-bound y)) 2))
; (+ (width-interval a) (width-interval b))

;;; 减法同上

(display-newline (width-interval (mul-interval (make-interval 1 2)
                                               (make-interval 1 3))))

(display-newline (width-interval (mul-interval (make-interval 0 1)
                                               (make-interval 1 3))))

;;; 上面两个区间乘法, 乘数宽度都是 1 2
;;; 但结果缺是 5/2, 3/2 所以区间乘法得到的宽度不是被乘区间的宽度的函数

;;; 除法同上 

(exit)

