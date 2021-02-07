(load "utils.ss")

;;; 通过 +1 -1 操作实现整数加法减法

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (plus-1 x y)
  (if (= x 0)
      y
      (inc (plus-1 (dec x) y))))

;;; 可以用 trace 追踪函数调用过程

(trace plus-1)

(display-newline (plus-1 3 4))

; (+ 3 4)
; (inc (+ 2 4))
; (inc (inc (+ 1 4)))
; (inc (inc (inc (+ 0 4))))
; (inc (inc (inc 4)))
; (inc (inc 5))
; (inc 6)
; 7
; 这是一个递归计算过程

(define (plus-2 x y)
  (if (= x 0)
      y
      (plus-2 (dec x) (inc y))))

(trace plus-2)

(display-newline (plus-2 3 4))

; (+ 3 4)
; (+ 2 5)
; (+ 1 6)
; (+ 0 7)
; 7
; 这是一个迭代计算过程

(exit)

