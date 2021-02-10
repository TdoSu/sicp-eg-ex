(load "utils.ss")

(define (f g) (g 2))

(f f)
;;; 会得到什么结果

;;; 用代换模型看一下
;;; (f f)
;;; (f 2)
;;; (2 2)
;;; 解释器会报错 2 不是一个过程
;;; Execption: attempt to apply non-procedure 2

(exit)

