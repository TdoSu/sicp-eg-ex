(load "utils.ss")

;;; 书中:
;;; 表示 --- 实现
;;; 使用 --- 使用

;;; 隔离数据的实现和使用 -- 将数据抽象为构造过程和选择过程
;;; 隔离数据的不同实现 -- 给数据添加类型标识

;;; 不同的复数实现方案

;;; 直角坐标

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

;;; 极坐标

(define (real-part z) (* (magnitude z) (cos (angle z))))

(define (imag-part z) (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) (cons r a))


;;; 复数运算

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (print-complex z)
  (begin (display (real-part z))
         (display "+")
         (display (imag-part z))
         (display "i")))

(print-complex (add-complex (make-from-real-imag 1 2) (make-from-real-imag 3 5)))
(newline)

(exit)

