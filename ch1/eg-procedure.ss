(load "utils.ss")

;;; 线性的递归和迭代

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(display-newline (factorial 6))

;;; 线性递归
; (factorial 6)
; ((lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))) 6)
; (if (= 6 0) 1 (* 6 (factorial (- 6 1))))
; (* 6 (factorial (- 6 1)))
; (* 6 (factorial 5))
; (* 6 (* 5 (factorial 4)))
; (* 6 (* 5 (* 4 (factorial 3))))
; (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (* 1 (factorial 0)))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (* 1 1))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
; (* 6 (* 5 (* 4 (* 3 2))))
; (* 6 (* 5 (* 4 6)))
; (* 6 (* 5 24))
; (* 6 120)
; 720

;;; 当计算 (fact 7) 的时候,
;;; 整个过程上下各加一步 -- 时间复杂度 O(n)
;;; 嵌套更深一层 -- 空间复杂度 O(n)

;;; product <-- counter * product
;;; counter <-- counter + 1

(define (factorial n)
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (- counter 1) (* counter product))))
  (iter n 1))

(display-newline (factorial 6))

;;; 线性迭代
; (factorial 6)
; (iter 6 1)
; (iter 5 6)
; (iter 4 30)
; (iter 3 120)
; (iter 2 360)
; (iter 1 720)
; (iter 0 720)
; 720

;;; 当计算 (fact 7) 的时候,
;;; 整个过程纵向加一步 -- 时间复杂度 O(n)
;;; 嵌套层级不变 -- 空间复杂度 O(1)

;;; 迭代过程是其状态可以用固定数目的变量描述的计算过程.
;;; 同时有一套规则可以描述从上一个状态到下一个状态的变化.

(exit)

