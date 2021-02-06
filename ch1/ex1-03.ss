(load "utils.ss")

; 写一个过程, 以三个数为参数, 返回较大两个数的平方和

(define (square x) (* x x))

(define (sum-of-squares a b) (+ (square a) (square b)))

; 思路一, 找到较大两个数, 求平方和后返回

(define (bigger-sum-of-squares a b c)
  (sum-of-squares (biggest-of-three a b c)
                  (second-biggest-of-three a b c)))

(define (biggest-of-three a b c)
  (bigger (bigger a b) c))

(define (bigger a b) (if (> a b) a b))

(define (second-biggest-of-three a b c)
  (define biggest (biggest-of-three a b c))
  (cond ((= biggest a) (bigger b c))
        ((= biggest b) (bigger a c))
        ((= biggest c) (bigger a b))))

; 思路二, 找到最小的数, 三数平方和后减去它的平方

(define (sum-of-squares-three a b c)
  (+ (square a) (square b) (square c)))

(define (bigger-sum-of-squares a b c)
  (- (sum-of-squares-three a b c) (square (smallest a b c))))

(define (smallest a b c)
  (smaller a (smaller b c)))

(define (smaller a b) (if (< a b) a b))

; 还可以有一些做法, 比如先求三个数平方, 然后求平方中最大两数和
; 或者直接扩展一下, 输入一个 list 排序, 然后求前 n 项平方和

(display-newline (bigger-sum-of-squares 3 4 5))
; > 41
(display-newline (bigger-sum-of-squares 4 4 5))
; > 41
(display-newline (bigger-sum-of-squares 4 5 5))
; > 50
(display-newline (bigger-sum-of-squares 5 5 5))
; > 50

(exit)

