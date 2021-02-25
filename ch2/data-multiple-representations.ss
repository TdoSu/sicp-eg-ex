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

;;; 构造带标识的数据

(define (attach-tag type-tag contents) (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error 'TYPE-TAG "Bad tagged datum" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error 'CONTENTS "Bad tagged datum" datum)))

;;; 区分直角坐标数据和极坐标数据

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;;; 区分同名过程

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle-rectangular z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z) (* (magnitude z) (cos (angle z))))

(define (imag-part-polar z) (* (magnitude z) (sin (angle z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                           (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;;; 根据类型实现通用的选择函数

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error 'REAL-PART "Unknown type" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error 'IMAG-PART "Unknown type" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error 'MAGNITUDE "Unknown type" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error 'ANGLE "Unknown type" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

(exit)

