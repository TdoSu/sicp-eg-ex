(load "utils.ss")

;;; 左折叠和右折叠

(define (accumulate op initial items)
  (if (null? items)
      initial
      (op (car items)
          (accumulate op initial (cdr items)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        ;;; 注意先 r 后 c
        ;;; js 的 reduce 就是左折叠
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(display-newline (fold-right / 1 (list 1 2 3)))
; (fold-right / 1 '(1 2 3))
; (/ 1 (/ 2 (/ 3 1)))
; 3/2

(display-newline (fold-left / 1 (list 1 2 3)))
; (iter 1 '(1 2 3))
; (iter 1 '(2 3))
; (iter 1/2 '(3))
; (iter 1/6 '())
; 1/6

(display-newline (fold-right list '() (list 1 2 3)))
; (fold-right list '() '(1 2 3))
; (list 1 (list 2 (list 3 '())))
; (1 (2 (3 ())))

(display-newline (fold-left list '() (list 1 2 3)))
; (iter '() '(1 2 3))
; (iter '(() 1) '(2 3))
; (iter '((() 1) 2) '(3))
; (iter '(((() 1) 2) 3) '())
; (((() 1) 2) 3)

;;; 什么样的 op, fold-right 和 fold-left 的结果相同呢?
;;; 首先 op 要满足交换律, 这样颠倒 r c 顺序就没有影响.
;;; 其次 op 要满足结合律, 这样先计算序列前面的还是后面的就没关系.

(exit)

