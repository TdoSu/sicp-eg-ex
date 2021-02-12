(load "utils.ss")

;;; 数据抽象
;;; 将数据的实现和数据的使用分离开

;;; 数据的实现 -- 构造函数, 选择函数

;;; 有理数的算数运算

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (begin (display (numer x))
         (display "/")
         (display (denom x))))

;;; 序对

;;; 有理数的表示 (实现)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define one-half (make-rat 1 2))
(print-rat one-half)
(newline)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(newline)
(print-rat (mul-rat one-half one-third))
(newline)
(print-rat (add-rat one-third one-third))
(newline)

;;; 数据意味着什么
;;; 一般而言数据是
;;; 一组构造函数和选择函数,
;;; 以及它们应该满足的特定条件。

;;; 比如有理数
;;; 构造函数 (make-rat n d) 选择函数 (numer r) (denom r)
;;; 条件 (= (/ n d) (/ (numer (make-rat n d)) (denom (make-rat n d))))
;;; 的值永远为 #t.

;;; 序对的定义
;;; 构造函数 cons 选择函数 car cdr
;;; 条件 (and (= x (car (cons x y))) (= y (cdr (cons x y)))) 的值永远是 #t

;;; 序对的一个实现
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error 'arguments "Argument not 0 or 1 -- CONS" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))

(display-newline (car (cons 3 4)))
(display-newline (cdr (cons 3 4)))

;;; 区间算术

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

(exit)

