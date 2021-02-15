(load "utils.ss")

;;; 闭包
;;; 一般说, 某种组合数据的操作, 如果组合后的结果还可以继续用这种操作组合,
;;; 那么就说这操作满足闭包特性.

;;; 序对就是闭包组合 (cons (cons 1 2) (cons 3 4))

;;; 满足闭包就可以构建层次性的结构.

;;; 序列
; (list a b c) -- 构造函数
; 等价于
; (cons a (cons b (cons c '())))
;;; 可以不断通过 car, cdr 拿到 list 的第一个值和其他值构成的 list -- 选择函数
;;; 可以通过 null? 判断表的结束

;;; 常用的表操作

(define (list-ref items n)
  (cond ((null? items) '())
        ((= n 0) (car items))
        (else (list-ref (cdr items) (- n 1)))))

(define squares (list 1 4 9 16 25))
(display-newline (list-ref squares 3))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length items)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ count 1))))
  (iter items 0))

(define odds (list 1 3 5 7))
(display-newline (length odds))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(display-newline (append (list 1 3 5) (list 2 4)))

;;; map 的实现

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

(display-newline (map square (list 1 2 3 4 5)))

;;; 求树的叶子数

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define x (cons (list 1 2) (list 3 4)))
;;; ((1 2) 3 4)
(display-newline (length x))
(display-newline (count-leaves x))
(display-newline (length (list x x)))
(display-newline (count-leaves (list x x)))

;;; 树的映射操作

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  ;;; 组合使用序列的 map 和递归
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(display-newline (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
                             10))

;;; 序列作为约定的界面

;;; 求树中奇数叶子的平方和
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

;;; 所有偶数斐波那契数
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;;; sum-odd-squares 和 even-fibs 将 生成数据, 映射, 过滤, 收集 这些操作混在了一起,
;;; 可以采用统一的数据切面分隔这些操作.

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate items)
  (cond ((null? items) '())
        ((predicate (car items))
         (cons (car items)
               (filter predicate (cdr items))))
        (else
          (filter predicate (cdr items)))))

(display-newline (filter odd? (list 1 2 3 4 5)))

(define (accumulate op initial items)
  (if (null? items)
      initial
      ;;; 从最右边开始折叠
      ;;; op 的第一个参数是当前值, 第二个参数是累计值
      ;;; (op c r)
      (op (car items)
          (accumulate op initial (cdr items)))))

(display-newline (accumulate + 0 (list 1 2 3 4 5)))
(display-newline (accumulate * 1 (list 1 2 3 4 5)))
(display-newline (accumulate cons '() (list 1 2 3 4 5)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1) high))))

(display-newline (enumerate-interval 2 7))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(display-newline (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))

;;; 用信号流的方式重新构造 sum-odd-squares 和 even-fibs

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(display-newline (sum-odd-squares (list 1 2 (list 3 (list 4 5) 6))))

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(display-newline (even-fibs 10))

;;; 嵌套映射

;;; 给定自然数 n, 找出所有不同的 (i j) 其中 1 <= j < i <= n,
;;; 并且 i + j 是素数

;;; 思路:
;;; 1. 找到所有序对 (i j) 
;;; 2. 过滤出加和是素数的
;;; 3. 映射成 (i j (+ i j))

(define (prime-sum-pairs n)
  (map (lambda (p) (list (car p) (cadr p) (+ (car p) (cadr p))))
       (filter (lambda (p) (prime? (+ (car p) (cadr p))))
               ;;; accumulate 目的是把里层 map 出来的 list 展开
               ;;; 所以使用 append
               (accumulate append
                           '()
                           ;;; 外层 map 获取 i
                           (map (lambda (i)
                                  ;;; 里层 map 获取 j
                                  (map (lambda (j) (list i j))
                                       (enumerate-interval 1 (- i 1))))
                                (enumerate-interval 1 n))))))

(display-newline (prime-sum-pairs 6))

;;; 数组展开
(define (flatmap proc seq) (accumulate append '() (map proc seq)))

;;; 求集合的所有排列
;;; 思路: 对每个元素 x, 生成 S-x 的全排列, 然后 x 放在所有全排列前面

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove x s)
  (filter (lambda (item) (not (= item x))) s))

(display-newline (permutations (list 1 2 3)))

(exit)

