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

(define (for-each-list proc items)
  (if (not (null? items))
      (begin (proc (car items))
             (for-each-list proc (cdr items)))))

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

;;; ------- 实现有理数运算  -----------

(define (gcd a b)
  (if (= a 0)
      b
      (gcd (remainder b a) a)))

; (display-newline (gcd 20 15))
; (display-newline (gcd 17 15))

;;; ---- constructor ----
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
;;; ---- selectors ----
(define (numer r) (car r))
(define (denom r) (cdr r))

(define (+rat r1 r2)
  (make-rat (+ (* (numer r1) (denom r2))
               (* (numer r2) (denom r1)))
            (* (denom r1) (denom r2))))

(define (*rat r1 r2)
  (make-rat (* (numer r1) (numer r2))
            (* (denom r1) (denom r2))))

(define (print-rat r)
  (begin (display (numer r))
         (display "/")
         (display (denom r))))

; (print-rat (+rat (make-rat 3 4) (make-rat 1 4)))
; (newline)
; (print-rat (*rat (make-rat 3 4) (make-rat 2 5)))
; (newline)

;;; 实现点和直线

(define (make-point x y) (cons x y))
(define (x-point z) (car z))
(define (y-point z) (cdr z))
(define (print-point z)
  (begin (display "(")
         (display (x-point z))
         (display ", ")
         (display (y-point z))
         (display ")")))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (print-segment s)
  (begin (print-point (start-segment s))
         (display "--")
         (print-point (end-segment s))))
(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (let ((x1 (x-point start))
          (y1 (y-point start))
          (x2 (x-point end))
          (y2 (y-point end)))
      (make-point (/ (+ x1 x2) 2)
                  (/ (+ y1 y2) 2)))))
(define (distance p1 p2)
  (let ((x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2)))
    (sqrt (+ (square (- x1 x2)) (square (- y1 y2))))))
(define (length-of-segment s)
  (distance (start-segment s) (end-segment s)))

; (print-point (make-point 3 5))
; (newline)
; (print-segment (make-segment (make-point 3 5) (make-point 1 2)))
; (newline)
; (print-point (midpoint-segment
;                (make-segment (make-point 3 5) (make-point 1 2))))
; (newline)
; (display-newline (distance (make-point 3 4) (make-point 0 0)))
; (display-newline (length-of-segment
;                    (make-segment (make-point 3 4)
;                                  (make-point 0 0))))

;;; ----- pairs -----
;;; constructor: cons    selector: car, cdr
;;; box and pointer notation
;;; For any x and y, (car (cons x y)) is x, (cdr (cons x y)) is y.

;;; 数据抽象就是把数据的使用和数据的实现分开.
;;; 实现这种分开的方法就是用 constructor 和 selector 构建一层屏障.

;;; *** WISHFUL THINKING ***
;;; --- 按愿望思考 (假设我们已经有了...)
;;; 然后我们可以给假设已经有了的东西取个名字代替真实的实现进行思考.

;;; ---- pairs 的一个实现方案 ----

(define (make-pair x y) (lambda (k) (if (= k 1) x y)))
(define (first-part-of-pair z) (z 1))
(define (second-part-of-pair z) (z 2))

; (display-newline (first-part-of-pair (make-pair 3 4)))
; (display-newline (second-part-of-pair (make-pair 3 4)))

;;; ---- 画图 ----

;;; 图像是一个过程, 给它一个矩形, 它可以在里面画东西

;;; 矩形 rect --- 一个原点和两个向量
(define (make-rect h v o) (list h v o))
(define (horiz-rect r) (car r))
(define (vert-rect r) (cadr r))
(define (origin-rect r) (caddr r))

;;; 向量操作
(define (make-vect x y) (cons x y))
(define (x-vect v) (car v))
(define (y-vect v) (cdr v))
(define (+vect v1 v2)
  (make-vect (+ (x-vect v1) (x-vect v2))
             (+ (y-vect v1) (y-vect v2))))
(define (scale-vect x v)
  (make-vect (* (x-vect v) x)
             (* (y-vect v) x)))

;;; 给定矩形返回一个过程, 这个过程可以把指定点映射到这个矩形内
(define (coord-map rect)
  (lambda (point)
    (+vect (+vect (scale-vect (x-point point)
                              (horiz-rect rect))
                  (scale-vect (y-point point)
                              (vert-rect rect)))
           (origin-rect rect))))

;;; 图像可以理解为线段列表, 线段可以由两个点定义
;;; Constructing Primitvie Pictures from Lists of Segments

(define (make-picture seglist)
  (lambda (rect)
    (for-each-list
      (lambda (s)
        ;;; drawline 假设系统内部已经实现了
        (drawline
          ((coord-map rect) (start-segment s))
          ((coord-map rect) (end-segment s))))
      seglist)))

; (define r (make-rect (make-vect 5 0)
;                      (make-vect 0 5)
;                      (make-point 0 0)))
; (define g (make-picture
;             (list (make-segment (make-point 1 2) (make-point 3 4))
;                   (make-segment (make-point 3 4) (make-point 2 5))
;                   (make-segment (make-point 2 5) (make-point 1 2)))))
; (g r)



;;; -------------------------- TODO --------------------------------

(exit)

