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

(exit)

