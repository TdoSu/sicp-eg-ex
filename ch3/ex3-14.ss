(load "utils.ss")

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        ;;; 暂存 x 的 cdr  --> temp
        (let ((temp (cdr x)))/
          ;;; 设置 x 的 cdr 为 y
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

(define w (mystery v))
;;; 翻转原数组
;;; 返回原数组第一个值

(display-newline w)
(display-newline v)

(exit)

