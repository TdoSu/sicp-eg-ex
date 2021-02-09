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

;;; 树形递归

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(display-newline (fib 5))
; (fib 5)
; (+ (fib 4) (fib 3))
; (+ (+ (fib 3) (fib 2)) (+ (fib 2) (fib 1)))
; (+ (+ (+ (fib 2) (fib 1)) (+ (fib 1) (fib 0)))
;    (+ (+ (fib 1) (fib 0)) (fib 1)))
; (+ (+ (+ (+ (fib 1) (fib 0)) (fib 1)) (+ (fib 1) (fib 0)))
;    (+ (+ (fib 1) (fib 0)) (fib 1)))
; (+ (+ (+ (+ 1 0) 1) (+ 1 0)) (+ (+ 1 0) 1))
; (+ (+ (+ (+ 1 0) 1) (+ 1 0)) (+ 1 1))
; (+ (+ (+ (+ 1 0) 1) (+ 1 0)) 2)
; (+ (+ (+ (+ 1 0) 1) 1) 2)
; (+ (+ (+ 1 1) 1) 2)
; (+ (+ 2 1) 2)
; (+ 3 2)
; 5
; 每次 n 加 1 会导致 fib(n-2) 被重新计算一次

;;; fib 的迭代实现
;;; a <-- a + b
;;; b <-- a
;;; a, b, counter 三个状态变量的迭代

(define (fib n)
  (define (iter a b counter)
    (if (= counter n)
        b
        (iter (+ a b) a (+ 1 counter))))
  (iter 1 0 0))

(display-newline (fib 5))
; (fib 5)
; (iter 1 0 0)
; (iter 1 1 1)
; (iter 2 1 2)
; (iter 3 2 3)
; (iter 5 3 4)
; (iter 8 5 5)
; 5

;;; 换零钱的方式统计

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((= kinds-of-coins 0) 0)
          (else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (cc amount 5))

(display-newline (count-change 100))
; 292

;;; 增长的阶
;;; 描述问题规模增加时, 占用计算机资源增加的情况.
;;; 问题规模 +1, 占用资源也 +1  -- O(n)
;;; 问题规模 +1, 占用资源加倍   -- O(a^n)
;;; 问题规模加倍, 占用资源 +1   -- O(loga(n))

;;; 快速幂

;;; 递归实现

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(display-newline (fast-expt 2 10))

;;; GCD -- 辗转相除法

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(display-newline (gcd (* 3 17 29 5) (* 5 17)))

;;; 素数检测

(exit)

