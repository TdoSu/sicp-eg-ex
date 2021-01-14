(define (display-newline . msgs)
  (if (null? msgs)
      (newline)
      (begin (display (car msgs))
             (display " ")
             (apply display-newline (cdr msgs)))))

(define (interval start end step)
  (if (> start end)
      '()
      (cons start (interval (+ start step) end step))))

(define (find-index-of-list items predicate?)
  (cond ((null? items) -1)
        ((predicate? (car items)) 0)
        (else (+ 1 (find-index-of-list (cdr items) predicate?)))))

(define (length-of-list items)
  (if (null? items)
      0
      (+ 1 (length-of-list (cdr items)))))

(define (ref-list items n)
  (cond ((null? items) '())
        ((= n 0) (car items))
        (else (ref-list (cdr items) (- n 1)))))

(define (map-list proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map-list proc (cdr items)))))

(define (filter-list predicate? items)
  (cond ((null? items) '())
        ((predicate? (car items))
         (cons (car items) (filter-list predicate? (cdr items))))
        (else (filter-list predicate? (cdr items)))))

(define (accumulate-list op init items)
  (if (null? items)
      init
      (op (car items) (accumulate-list op init (cdr items)))))

(define (append-list l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append-list (cdr l1) l2))))

;;; string->list list->string
;;; string-ref string-length string-append

;;; -----------------------------------------------------------

;;; processes 和 procedures
;;; 计算机里的精灵和咒语

;;; techniques for controlling the complexity of these large systems
;;; 1. black-box abstraction
;;; 2. conventional interfaces
;;; 3. metaliguistic abstraction

;;; 当别人向你展示一门语言时, 你首先要问的是:
;;; 1. what is primitive elements ?
;;; 2. what is means of combination ?
;;; 3. what is means of abstraction ?

;;; combination (+ 3 17.9 5)
;;;   operator +
;;;   operands 3 17.9 5
;;; combination 可以作为其他 combination 的 operands 或者 operator
;;; operator 在最左边 --- prefix notation
;;; 一个 combination 等价于一颗 tree
;;; (+ 3 (* 5 6) 8 2) --> 43
;;;     +
;;;     3
;;;     (* 5 6) --> 30
;;;         *
;;;         5
;;;         6
;;;     8
;;;     2

;;; define 可以给一个值取一个名字 (组合式等价于值)
;;; (define a (* 5 5))
;;; (* a a) --> 625
;;; (define b (+ a (* 5 a)))

;;; 表示 (* 5 5) (* 1001 1001) (* 6 6) (* 1001.7 1001.7)
;;; (define (square x)          (* x x))
;;;   to    square something is mutiply it by itself
;;; (define square (lambda (x) (* x x)))
;;;         square 表示   一个数 和它自身相乘
;;; lambda 构造一个 procedure, procedure 分 arguments 和 body 两个部分

;;; ---- syntactic sugar
;;; 两种等价写法, 一种比另一种更好写而已.

;;; ----- 海伦法求平方根 -----

(define (square x) (* x x))

(define (abs x) ((if (> x 0) + -) x))

(define (average . numbers)
  (define (sum-of-list items) (accumulate-list + 0 items))
  (/ (sum-of-list numbers) (length-of-list numbers)))

(define (sqrt x)
  (define (good-enough? g)
    (< (abs (- (square g) x)) 0.00001))
  (define (improve g)
    (average (/ x g) g))
  (define (try g)
    (if (good-enough? g)
        g
        (try (improve g))))
  (try 1.0))

; (display-newline (sqrt (square 3)))

;;; *** black-box ***
;;; sqrt
;;;     -> try (-> try itself)
;;;         -> good-enough?
;;;             -> abs
;;;             -> square
;;;         -> improve
;;;             -> average

;;; recursive definition

;;; block structure (from ALGOL 60)
;;; this particular way of packaging internals inside of a definition
;;; 有两个特点:
;;; 1. 过程内部可以定义过程, 内部过程的名字被限制在定义中
;;; 2. 内部过程里面可以访问外部过程中的其他定义 -- 词法作用域

;;; lisp
;;; ----------------------------------------------
;;;                      | procedures |  data
;;; primitive elements   | + * < =    | 23 1.738
;;; means of combination | () cond if
;;; means of abstraction | define
;;; ----------------------------------------------

;;; kinds of expressions
;;; ------------------------------------
;;; numbers
;;; symbols
;;; lambda expressions  -- special forms
;;; definitions         -- special forms
;;; conditionals        -- special forms
;;; conbinations

;;; substitution rule - to evaluate an application
;;; ------------------------------------------------
;;; 1. evaluate the operator to get procedure
;;; 2. evaluate the operands to get arguments
;;; 3. apply the procedure to the arguments
;;;   copy the body of the procedure,
;;;   substituting the arguments supplied
;;;   for the formal parameters of the procedure.
;;;   Evaluate the resulting new body.

(define (sum-of-squares x y) (+ (square x) (square y)))

;;; (sum-of-squares 3 4)
;;; ((lambda (x y) (+ (square x) (square y))) 3 4)
;;; (+ (square 3) (square 4))
;;; (+ (square 3) ((lambda (x) (* x x)) 4))
;;; (+ (square 3) (* 4 4))
;;; (+ (square 3) 16)
;;; (+ ((lambda (x) (* x x)) 3) 16)
;;; (+ (* 3 3) 16)
;;; (+ 9 16)
;;; 25

;;; (IF <predicate> <consequent> <alternative>)
;;; to evaluate an IF expression
;;; -----------------------------
;;; Evaluate the predicate expression
;;; if it yields TRUE
;;;   evaluate the consequent expression
;;; otherwise
;;;   evaluate the alternative expression

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

;;; 迭代实现
(define (add-integer x y)
  (if (= x 0)
      y
      (add-integer (dec x) (inc y))))

; (display-newline (add-integer 3 4))

;;; (add-integer 3 4)
;;; (add-integer 2 5)
;;; (add-integer 1 6)
;;; (add-integer 0 7)
;;; 7

;;; 递归实现
(define (add-integer x y)
  (if (= x 0)
      y
      (inc (add-integer (dec x) y))))

; (display-newline (add-integer 3 4))

;;; (add-integer 3 4)
;;; (inc (add-integer 2 4))
;;; (inc (inc (add-integer 1 4)))
;;; (inc (inc (inc (add-integer 0 4))))
;;; (inc (inc (inc 4)))
;;; (inc (inc 5))
;;; (inc 6)
;;; 7

;;; 斐波那契数 - 递归计算过程
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;;; 斐波那契数 - 迭代计算过程
(define (fib n)
  (define (iter a b counter)
    (if (= counter n)
        a
        (iter b (+ a b) (+ counter 1))))
  (iter 0 1 0))

; (display-newline (fib 1))
; (display-newline (fib 2))
; (display-newline (fib 3))
; (display-newline (fib 4))
; (display-newline (fib 5))
; (display-newline (fib 6))

;;; 汉诺塔
(define (move n from to spare)
  (define (print-move from to)
    (display-newline from "-" to))
  (cond ((= n 0) "DONE")
        (else
          (move (- n 1) from spare to)
          (print-move from to)
          (move (- n 1) spare to from))))

; (move 4 'from 'to 'spare)

;;; 对于求和过程的抽象 -- 过程作为参数

(define (sum term start next end)
  (if (> start end)
      0
      (+ (term start) (sum term (next start) next end))))

(define (sum-integer start end)
  (sum (lambda (x) x) start (lambda (x) (+ x 1)) end))
; (display-newline (sum-integer 1 100))

(define (sum-squares start end)
  (sum square start (lambda (x) (+ x 1)) end))
; (display-newline (sum-squares 1 4))

(define (pi-sum start end)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       start
       (lambda (x) (+ x 4))
       end))
; (display-newline (* 8 (pi-sum 1 100000)))

;;; ----- 利用不动点思想求平方根 (过程作为返回值) -----
;;; improve 我们的改进函数其实就是要找的不动点函数,
;;; 当不需要再改进时, 其实就是不动点,
;;; 这就是不动点和 try-improve-good-enoughe 之间的关系.

(define (fixed-point f start)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.00001))
  (define (next g) (f g))
  (define (guess g)
    (if (close-enough? g (next g))
        (next g)
        (guess (next g))))
  (guess start))

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

; (display-newline (sqrt (square 3)))

; (display-newline (fixed-point cos 1.0))
;;; -> 0.7390822985224024

;;; ------ 利用牛顿法求平方根 (过程作为返回值) -------

(define (newtown-method f guess)
  (define dx 0.000001)
  (define (deriv f)
      (lambda (x) (/ (- (f (+ x dx)) (f x))dx)))
  (define df (deriv f))
  (fixed-point
    (lambda (x) (- x (/ (f x) (df x))))
    1.0))

(define (sqrt x)
  (newtown-method (lambda (y) (- (square y) x)) 1.0))

; (display-newline (sqrt (square 3)))

;;; the rights and privileges of first-class citizens
;;; To be named by variable.
;;; To be passed as arguments to procedures.
;;; To be returned as values of procedures.
;;; To be incorporated into data structures.


;;; -------------------------- TODO --------------------------------

(exit)

