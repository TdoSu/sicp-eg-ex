(load "utils.ss")

;;; 序对的另一种实现

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z) (z (lambda (x y) x)))

(define (cdr z) (z (lambda (x y) y)))

(display-newline (car (cons 3 4)))
(display-newline (cdr (cons 3 4)))

; (car (cons a b))
; (car (lambda (m) (m a b)))
; ((lambda (m) (m a b)) (lambda (x y) x))
; ((lambda (x y) x) a b)
; a

; (cdr (cons a b))
; (cdr (lambda (m) (m a b)))
; ((lambda (m) (m a b)) (lambda (x y) y))
; ((lambda (x y) y) a b)
; b

(exit)

