(load "utils.ss")

;;; 非确定性求值器

;;; 程序只是对问题的描述
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    ;;; 求值器自己在一个范围内搜索结果
    (require (prime? (+ a b)))
    (list a b)))

;;; Amb-Eval input:
; (prime-sum-pair '(1 3 5 8) '(20 35 110))
;;; Starting a new problem
;;; Amb-Eval value:
; (3 20)

;;; Amb-Eval input:
; try-again
;;; Amb-Eval value:
; (3 110)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(exit)

