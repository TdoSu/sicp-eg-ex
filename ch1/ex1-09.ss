(load "utils.ss")

;;; 通过 +1 -1 操作实现整数加法减法

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (+ x y)
  (if (= x 0)
      y
      (inc (+ (dec x) y))))

(display-newline (+ 3 4))

; (+ 3 4)
; (inc (+ 2 4))
; (inc (inc (+ 1 4)))
; (inc (inc (inc (+ 0 4))))
; (inc (inc (inc 4)))
; (inc (inc 5))
; (inc 6)
; 7
; 这是一个递归计算过程

(define (+ x y)
  (if (= x 0)
      y
      (+ (dec x) (inc y))))

(display-newline (+ 3 4))

; (+ 3 4)
; (+ 2 5)
; (+ 1 6)
; (+ 0 7)
; 7
; 这是一个迭代计算过程

(exit)

