(load "utils.ss")

;;; Church 计数

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;; 定义 one
; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

;;; 定义 two
; (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;;; 定义 +

(define (1+ n)
  (lambda (f)
    ;;; 下面这个写法展开后就会在 n 里面多加入一层 f
    ;;; 每个数字都是 f 和 x 的嵌套
    ;;; 最外层是 f 最里层是 x
    (lambda (x) (f ((n f) x)))))

(define (+ m n) (compose m n))

(exit)

