(load "utils.ss")

; 1. 全局环境准备好

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
; 2. 创建一个过程
; 过程: 一个指针指向参数和过程体, 一个指针指向全局环境
; 环境中一个名字 factorial 指针指向这个过程

(factorial 6)

; 3. 计算 (factorial 6)
; 创建环境 E1, 框架中名字 n --> 6, 环境指针指向全局环境 (factorial 定义的环境)
; 求值 (if (= n 1) (* n (factorial (- n 1))))
; n 在 E1 中绑定, 值是 6
; factorial 在全局中绑定
; --> (* 6 (factorial 5))

; 4. 计算 (factorial 5)
; 创建环境 E2, 框架中名字 n --> 5, 环境指针指向全局环境 (factorial 定义的环境)
; 求值 (if (= n 1) (* n (factorial (- n 1))))
; n 在 E2 中绑定, 值是 5
; factorial 在全局中绑定
; --> (* 5 (factorial 4))

; 5. 计算 (factorial 4)
; 创建环境 E3, 框架中名字 n --> 4, 环境指针指向全局环境 (factorial 定义的环境)
; 求值 (if (= n 1) (* n (factorial (- n 1))))
; n 在 E3 中绑定, 值是 4
; factorial 在全局中绑定
; --> (* 4 (factorial 3))

; 6. 计算 (factorial 3)
; 创建环境 E4, 框架中名字 n --> 3, 环境指针指向全局环境 (factorial 定义的环境)
; 求值 (if (= n 1) (* n (factorial (- n 1))))
; n 在 E4 中绑定, 值是 3
; factorial 在全局中绑定
; --> (* 3 (factorial 2))

; 7. 计算 (factorial 2)
; 创建环境 E5, 框架中名字 n --> 2, 环境指针指向全局环境 (factorial 定义的环境)
; 求值 (if (= n 1) (* n (factorial (- n 1))))
; n 在 E4 中绑定, 值是 2
; factorial 在全局中绑定
; --> (* 2 (factorial 1))

; 8. 计算 (factorial 1)
; 创建环境 E6, 框架中名字 n --> 1, 环境指针指向全局环境 (factorial 定义的环境)
; 求值 (if (= n 1) (* n (factorial (- n 1))))
; n 在 E4 中绑定, 值是 1
; factorial 在全局中绑定
; --> 1

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(factorial 6)

; 1. 定义过程 factorial
; 2. 定义过程 fact-iter
; 3. 调用 (factorial 6) E1
; 4. 调用 (fact-iter 1 1 6) E2
; 5. 调用 (fact-iter 1 2 6) E3
; 6. 调用 (fact-iter 2 3 6) E4
; 7. 调用 (fact-iter 6 4 6) E5
; 8. 调用 (fact-iter 24 5 6) E6
; 9. 调用 (fact-iter 120 6 6) E7
; 10. 调用 (fact-iter 720 7 6) E8


(exit)

