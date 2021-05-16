(load "utils.ss")

;;; 猜测下面这些表达式的值

(display-newline 10)
; > 10
; 数字表达式是基本的表达式, 它的值就是它本身

(display-newline (+ 5 3 4))
; > 12
; 这是一个组合式
; 组合式的值是把最右边的操作符用于每个操作对象
; + 是基本过程, 对所有参数求和

(display-newline (- 9 1))
; > 8
; 这是一个组合式
; 组合式的值是把最右边的操作符用于每个操作对象
; - 是基本过程, 有多个参数时, 第一个参数减去后续所有参数, 只有一个参数, 返回其相反数

(display-newline (/ 6 2))
; > 3
; 这是一个组合式
; 组合式的值是把最右边的操作符用于每个操作对象
; / 是基本过程, 有多个参数时, 第一个参数除以后续所有参数, 只有一个参数, 返回其倒数

(display-newline (+ (* 2 4) (- 4 6)))
; 组合式求值
; (+ (* 2 4) -2)
; (+ 8 -2)
; > 6

(define a 3)
; a --> 3
; 返回的值是未定义的值 (标准没要求, 各个解释器实现不一样)
; mit 解释器据说返回的是 a, chez scheme 返回空
; 如果用 display 打印会报错
; Exception: invalid context for definition (define a 3)
; (define a 3) 上下文非法
; 测试了一下, define 出现在 lambda 中不会报错, 作为其他表达式的参数都会报错
; (lambda ()
;   (define a 3)
;   4)

(define b (+ a 1))
; b --> 4

(display-newline (+ a b (* a b)))
; (+ 3 4 (* 3 4))
; (+ 3 4 12)
; > 19

(display-newline (= a b))
; = 返回布尔值 (= 3 4)
; > #f

(display-newline (if (and (> b a) (< b (* a b))) b a))
; if 是一个特殊形式
; 首先求值 (and (> b a) (< b (* a b)))
; and 也是特殊形式, 求值 (> b a) --> (4 3) #t
; 然后求值 (< b (* a b)) --> #t
; 得到 if 的第一个参数是 #t
; 求值 b --> 4
; > 4

(display-newline (cond ((= a 4) 6)
                       ((= b 4) (+ 6 7 a))
                       (else 25)))
; cond 也是特殊形式
; (= a 4) --> #f
; (= b 4) --> #t
; (+ 6 7 a) --> 16
; > 16

(display-newline (+ 2 (if (> b a) b a)))
; (+ 2 (if #t b a))
; (+ 2 b)
; (+ 2 4)
; > 6

(display-newline (* (cond ((> a b) a)
                          ((< a b) b)
                          (else -1))
                    (+ a 1)))
; (* (cond ((> a b) a) ((< a b) b) (else -1)) (+ a 1))
; (* (cond ((> a b) a) ((< a b) b) (else -1)) (+ 3 1))
; (* (cond ((> a b) a) ((< a b) b) (else -1)) 4)
; (* (cond (#f a) ((< a b) b) (else -1)) 4)
; (* (cond (#f a) (#t b) (else -1)) 4)
; (* b 4)
; (* 4 4)
; > 16

(exit)

