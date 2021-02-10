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

;;; 递归实现 O(log(n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(display-newline (fast-expt 2 10))

;;; GCD -- 辗转相除法 O(log(n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(display-newline (gcd (* 3 17 29 5) (* 5 17)))

;;; 素数检测

;;; 寻找最小因子 O(sqrt(n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (and (> n 1) (= (smallest-divisor n) n)))

;;; 费马检测 O(log(n))

;;; 如果 p 那么 q === 不存在 p 并且 非q 的情况
(define (if-then? p q) (not (and p (not q))))

; 费马小定理:
; 如果 n 是一个素数, a 是小于 n 的任意正整数, 那么 a 的 n 次方与 a 模 n 同余.
; (lambda (a)
;   (if-then? (and (prime? n) (integer? a) (< a n))
;             (= (remainder (exp a n) n) (remainder a n))))
; --> #t 永远为真

;;; 计算一个数的幂对另一个数求余数
;;; 这里利用了 remainder 的一个特点
;;; (lambda (a b)
;;;   (= (remainder a b) (remainder (remainder a b) b)))
;;; --> #t 永远为真
;;; 这样就可以在求幂的过程中, 用 remainder 缩小 a 的值
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

; (display-newline (expmod 2 3 3))

;;; 因为 (< a n) 是 #t 所以费马检测等价于
;;; 1 ~ (- n 1) 之间所有整数的 n 次幂对 n 求余数都等于 a

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  ; (random x) --> a nonnegative pseudo-random number less than x
  ; x 是整数得到的就是整数, x 是小数得到的就是小数
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(display-newline (fast-prime? 13 10))
(display-newline (fast-prime? (* 13 17) 10))

(exit)

