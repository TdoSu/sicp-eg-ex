(load "utils.ss")

;;; 实现 double 接收一个过程, 返回执行两次这过程的过程
;;; 参数过程只有一个参数

(define (double f)
  (lambda (x) (f (f x))))

(display-newline ((double inc) 3))

(display-newline (((double (double double)) inc) 5))

; (((double (double double)) inc) 5)
; (((double (lambda (x) (double (double x)))) inc) 5)
; (((lambda (x)
;     ((lambda (x) (double (double x)))
;      ((lambda (x) (double (double x))) x)))
;   inc)
;  5)
; (((lambda (x)
;     ((lambda (x) (double (double x)))
;      (double (double x))))
;   inc)
;  5)
; (((lambda (x)
;     (double (double (double (double x)))))
;   inc)
;  5)
; ((double (double (double (double inc)))) 5)
; ((double (double (double 2+))) 5)
; ((double (double 4+)) 5)
; ((double 8+) 5)
; (16+ 5)
; 21

(exit)

