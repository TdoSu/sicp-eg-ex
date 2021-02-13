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

(exit)

