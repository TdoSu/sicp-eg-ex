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
(define (make-rect o h v) (list o h v))
(define (origin-rect r) (car r))
(define (horiz-rect r) (cadr r))
(define (vert-rect r) (caddr r))

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

;;; 图形操作就变成了对矩形的变换

(define (beside-picture p1 p2 a)
  (lambda (rect)
    (p1 (make-rect
          (origin-rect rect)
          (scale-vect a (horiz-rect rect))
          (vert-rect rect)))
    (p2 (make-rect
          (+vect (origin-rect rect)
                 (scale a (horiz-rect rect)))
          (scale (- 1 a) (horiz-rect rect))
          (vert-rect rect)))))

(define (rotate90-picture p)
  (lambda (rect)
    (p (make-rect
         (+vect (origin-rect rect)
                (horiz-rect rect))
         (vert-rect rect)
         (scale -1 (horiz-rect rect))))))

;;; All right, so you're using the procedural representation
;;; to ensure the closure.

;;; What I mean is by embedding the language in this way,
;;; all the power of Lisp is automatically available
;;; as an extension to whatever you want to do.

;;; 这样我们自然就有了 Lisp 组合过程的能力.

(define (right-push-picture p n a)
  (if (= n 0)
      p
      (beside-picture p
                      (right-push p (- n 1) a)
                      a)))

(define (push-picture comb)
  (lambda (pict n a)
    ((repeated
       (lambda (p) (comb pict p a))
       n)
     pict)))

(define (combine-proc f g)
  (lambda (x) (f (g x))))

(define (repeated p n)
  (if (= n 1)
      p
      (combine-proc p (repeated p (- n 1)))))

; (display-newline ((repeated (lambda (x) (+ x 2)) 3) 3))

(define right-push (push-picture beside-picture))

;;; ------- 符号求导 --------

(define (deriv-expr expr var)
  (define (zero? x) (and (number? x) (= x 0)))
  (define (one? x) (and (number? x) (= x 1)))
  ;;; 常量
  (define (constant? expr var)
    (and (atom? expr)
         (not (eq? expr var))))
  ;;; 同一个变量
  (define (same-var? expr var)
    (and (atom? expr)
         (eq? expr var)))
  ;;; 和
  (define (sum-expr? expr)
    (and (not (atom? expr))
         (eq? (car expr) '+)))
  (define (make-sum-expr a1 a2)
    (cond ((zero? a1) a2)
          ((zero? a2) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (a1-sum-expr expr) (cadr expr))
  (define (a2-sum-expr expr) (caddr expr))
  ;;; 积
  (define (product-expr? expr)
    (and (not (atom? expr))
         (eq? (car expr) '*)))
  (define (make-product-expr m1 m2)
    (cond ((or (zero? m1) (zero? m2)) 0)
          ((one? m2) m1)
          ((one? m1) m2)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (m1-product-expr expr) (cadr expr))
  (define (m2-product-expr expr) (caddr expr))
  ;;; 程序主体
  (cond ((constant? expr var) 0)
        ((same-var? expr var) 1)
        ((sum-expr? expr)
         (make-sum-expr (deriv-expr (a1-sum-expr expr) var)
                        (deriv-expr (a2-sum-expr expr) var)))
        ((product-expr? expr)
         (make-sum-expr (make-product-expr
                          (m1-product-expr expr)
                          (deriv-expr (m2-product-expr expr) var))
                        (make-product-expr
                          (m2-product-expr expr)
                          (deriv-expr (m1-product-expr expr) var))))
        ;;; 其他规则
        ))

; (define foo
;   '(+ (* a (* x x))
;       (+ (* b x)
;          c)))
; (display-newline (deriv-expr foo 'x))
; (display-newline (deriv-expr foo 'a))
; (display-newline (deriv-expr foo 'b))
; (display-newline (deriv-expr foo 'c))

;;; -------------- 模式实现符号求导 -----------------

;;; 我们为什么要把这些规则翻译成计算机语言呢?
;;; 按表达式类型做分派 (dispatch) 的分情况分析语句
;;; 按类型分派
;;; 有没有其他办法把程序写的更清晰一些呢.
;;; 规则分左右两个部分, 左边的部分用来判断和匹配, 右边替换原来的表达式.

;;; pattern ----- rule ----> skeleton
;;;   |                         |
;;;   |                         |
;;;   match                   instantiation
;;;   |                         |
;;;   |                         |
;;;   V                         V
;;; expression -------------> exrpession
;;; source                    target

;;; 构建一种语言实现描述规则,
;;; 并且实现这个语言的解释和执行(翻译成可运行的计算机程序)

(define deriv-rules
  '(
    ;;; 前面部分是 pattern (match), 后面部分是 skeleton (instantiation)
    ;;; match 判断是否匹配, 并且抽取各个部分
    ;;; instantiation 把 match 抽取的部分填入 skeleton
    ((dd (?c c) (? v))    0)
    ((dd (?v v) (? v))    1)
    ((dd (?v u) (? v))    0)
    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x2) (: v))))
    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (? v)))
        (* (: x2) (dd (: x1) (? v)))))
    ((dd (** (? x) (?c n)) (? v))
     (* (* (: n)
           (** (: x) (: (- n 1))))
        (dd (: x) (: v))))
  ))

;;; pattern match
;;; foo --- matches exactly foo
;;; (f a b) --- matches that first element is f, second a, third b
;;; (? x) --- matches anything called x
;;; (?c x) --- matches constant called x
;;; (?v x) --- matches variable called x

;;; skeleton
;;; foo --- foo, a symbol, instantion to itself
;;; (f a b) --- instantiates to a 3-list, f, a, b
;;; (: x) --- instantiates to the value of x as in the matched pattern

(define algebra-rules
  '(
    (((? op) (?c e1) (?c e2))
     (: (op e1 e2)))
    ;;; 如果是除法这条规则是有漏洞的
    (((? op) (? e1) (?c e2))
     ((: op) (: e2) (: e1)))
    ((+ 0 (? e)) (: e))
    ((* 1 (? e)) (: e))
    ((* 0 (? e)) 0)
    ((* (?c e1) (* (?c e2) (? e3)))
     (* (: (* e1 e2)) (: e3)))
    ((* (? e1) (* (?c e2) (? e3)))
     (* (: e2) (* (: e1) (: e3))))
    ((* (* (? e1) (? e2)) (? e3))
     (* (: e1) (* (: e2) (: e3))))
    ((* (?c e1) (* (?c e2) (? e3)))
     (* (: (* e1 e2)) (: e3)))
  ))

(define (match pat expr dict)
  (cond ((eq? dict 'failed) 'failed)
        ;;; 这里判断基本元素是否匹配
        ((atom? pat)
         ;;; Atomic patterns
         (if (atom? expr)
             (if (eq? pat expr)
                 dict
                 'failed)
             'failed))
        ;;; Pattern variable clauses
        ;;; ?c
        ((arbitrary-constant? pat)
         (if (constant? expr)
             (extend-dict pat expr dict)
             'failed))
        ;;; ?v
        ((arbitrary-variable? pat)
         (if (variable? expr)
             (extend-dict pat expr dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dict pat expr dict))
        ((atom? expr) 'failed)
        ;;; 这里判断通用规则
        (else
          (match (cdr pat)
                 (cdr expr)
                 (match (car pat)
                        (car expr)
                        dict)))))

(define (instantiate skeleton dict)
  (define (evaluate form dict)
    (if (atom? form)
        (lookup form dict)
        (apply
          (eval (lookup (car form) dict)
                user-initial-environment)
          (mapcar (lambda (v)
                    (lookup v dict))
                  (cdr form)))))
  (define (loop s)
    (cond ((atom? s) s)
          ;;; : 表达式的替换
          ((skeleton-evaluation? s)
           (evaluate (eval-expr s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (simplifier the-rules)
  (define (simplify-expr expr)
    (try-rules (if (compound? expr)
                   (map-list simplify-expr expr)
                   expr)))
  (define (try-rules expr)
    (define (scan rules)
      (if (null? rules)
          expr
          (let ((dict
                  (match (pattern (car rules))
                         expr
                         (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-expr
                  (instantiate
                    (skeleton (car rules))
                    dict))))))
    (scan the-rules))
  simplify-expr)

(define (empty-dictionary) '())

(define (extend-dict pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((null? v)
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v) var (cadr v))))

; (define dsimp
;   (simplifier deriv-rules))

;;; (display-newline (dsimp '(dd (+ x y) x)))
;;; => (+ 1 0)

;;; ---------- 通用运算 ------------
;;; 通用操作的意思是, 一个操作根据数据的种类决定做什么.
;;; Generic operator means what sort of precisely does depends
;;; on the kind of data that it's looking at.
;;; 数据抽象 -- 数据 api 是横向分层
;;; 数据类型 -- 是纵向分层

;;; Arithmetic operations on complex numbers

(define (+c z1 z2)
  (make-rectangular-c (+ (real-part-c z1) (real-part-c z2))
                      (+ (imag-part-c z1) (imag-part-c z2))))
(define (-c z1 z2)
  (make-rectangular-c (- (real-part-c z1) (real-part-c z2))
                      (- (imag-part-c z1) (imag-part-c z2))))
(define (*c z1 z2)
  (make-polar-c (* (magnitude-c z1) (magnitude-c z2))
                (+ (angle-c z1) (angle-c z2))))
(define (/c z1 z2)
  (make-polar-c (/ (magnitude-c z1) (magnitude-c z2))
                (- (angle-c z1) (angle-c z2))))

;;; selectors
(define (real-part-c-rectangular z) (car z))
(define (imag-part-c-rectangular z) (cdr z))
(define (magnitude-c-rectangular z)
  (sqrt (+ (square (car z)) (square (cdr z)))))
(define (angle-c-rectangular z) (atan (cdr z) (car z)))

;;; constructors
(define (make-rectangular-c x y)
  (attach-type 'rectangular (cons x y)))
; (define (make-polar-c r a) (cons (* r (cos a)) (* r (sin a))))

;;; selectors
(define (real-part-c-polar z) (* (car z) (cos (cdr z))))
(define (imag-part-c-polar z) (* (car z) (sin (cdr z))))
(define (magnitude-c-polar z) (car z))
(define (angle-c-polar z) (cdr z))

;;; constructors
; (define (make-rectangular-c x y)
;   (cons (sqrt (+ (square x) (square y)))
;         (atan (/ y x))))
(define (make-polar-c r a)
  (attach-type 'polar (cons r a)))

;;; TYPED DATA (type + content)

;;; Support mechanism for manifest types

(define (attach-type type contents) (cons type contents))
(define (type datum) (car datum))
(define (contents datum) (cdr datum))

;;; type predicates

(define (rectangular? z) (eq? (type z) 'rectangular))
(define (polar? z) (eq? (type z) 'polar))

;;; generic selectors for complex numbers

(define (real-part-c z)
  (cond ((rectangular? z) (real-part-c-rectangular (contents z)))
        ((polar? z) (real-part-c-polar (contents z)))))

(define (imag-part-c z)
  (cond ((rectangular? z) (imag-part-c-rectangular (contents z)))
        ((polar? z) (imag-part-c-polar (contents z)))))

(define (magnitude-c z)
  (cond ((rectangular? z) (magnitude-c-rectangular (contents z)))
        ((polar? z) (magnitude-c-polar (contents z)))))

(define (angle-c z)
  (cond ((rectangular? z) (angle-c-rectangular (contents z)))
        ((polar? z) (angle-c-polar (contents z)))))

;;; 上面的方式叫做 DISPATCH ON TYPE
;;; 这个系统的问题时, 需要一个经理做大量分派的工作.
;;; 而这个工作完全可以被查表代替.

; (put key1 key2 value)
; (get key1 key2) => value

;;; Installing the rectangular
;;; operation in the table

; (put 'rectangular 'real-part real-part-c-rectangular)
; (put 'rectangular 'imag-part imag-part-c-rectangular)
; (put 'rectangular 'magnitude magnitude-c-rectangular)
; (put 'rectangular 'angle angle-c-rectangular)

;;; Installing the polar
;;; operation in the table

; (put 'polar 'real-part real-part-c-polar)
; (put 'polar 'imag-part imag-part-c-polar)
; (put 'polar 'magnitude magnitude-c-polar)
; (put 'polar 'angle angle-c-polar)

(define (operate op obj)
  (let ((proc (get (type obj) op)))
    (if (not (null? proc))
        (proc (contents obj))
        (error "undefined operator" op))))

;;; Defining the selectors using operate

(define (real-part-c obj) (operate 'real-part obj))
(define (imag-part-c obj) (operate 'imag-part obj))
(define (magnitude-c obj) (operate 'magnitude obj))
(define (angle-c obj) (operate 'angle obj))

;;; 上面的实现叫做 DATA-DIRECTED PROGRAMMING
;;; 让数据携带着如何操作他们的信息.

;;; MESSAGE PASSING 风格
;;; 让数据直接携带操作他们的方法.

;;; installing rational numbers in the generic arithmetic system

(define (make-rat x y)
  (attach-type 'rational (cons x y)))

; (put 'rational 'add +rat)
; (put 'rational 'sub -rat)
; (put 'rational 'mul *rat)
; (put 'rational 'div /rat)

(define (add x y) (operate-2 'add x y))

(define (operate-2 op arg1 arg2)
  (if (eq? (type arg1) (type arg2))
      (let ((proc (get (type arg1) op)))
        (if (not (null? proc))
            (proc (contents arg1)
                  (contents arg2))
            (error "Op undefined on type" op)))
      (error "Args not same type" arg1 arg2)))

;;; installing complex numbers

(define (make-complex z) (attach-type 'complex z))
(define (+complex z1 z2) (make-complex (+c z1 z2)))
(define (-complex z1 z2) (make-complex (-c z1 z2)))
(define (*complex z1 z2) (make-complex (*c z1 z2)))
(define (/complex z1 z2) (make-complex (/c z1 z2)))

; (put 'complex 'add +complex)
; (put 'complex 'sub -complex)
; (put 'complex 'mul *complex)
; (put 'complex 'div /complex)

;;; installing ordinary numbers

(define (make-number n) (attach-type 'number n))
(define (+number x y) (make-number (+ x y)))
(define (-number x y) (make-number (- x y)))
(define (*number x y) (make-number (* x y)))
(define (/number x y) (make-number (/ x y)))

; (put 'number 'add +number)
; (put 'number 'sub -number)
; (put 'number 'mul *number)
; (put 'number 'div /number)

;;; 类型的链条, 在表格的纵向屏障中为你指路.
;;; 一层层拆掉类型, 一层层添加类型.

;;; installing polynomials

(define (make-polynomial var term-list)
  (attach-type 'polynomial
               (cons var term-list)))

(define (+poly p1 p2)
  (if (same-var? (var p1) (var p2))
      (make-polynomial
        (var p1)
        (+terms (term-list p1)
                (term-list p2)))
      (error "Polys not in same var" p1 p2)))

; (put 'polynomial 'add +poly)

(define (+terms l1 l2)
  (define (add-term t1 t2)
    (make-term (order t1)
               ;;; 这个通用运算的 add 是关键
               (add (coeff t1)
                    (coeff t2))))
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
        (else
          (let ((t1 (first-term l1))
                (t2 (first-term l2)))
            (cond ((> (order t1) (order t2))
                   (adjoin-term t1 (+terms (rest-terms l1) l2)))
                  ((< (order t1) (order t2))
                   (adjoin-term t2 (+terms l1 (rest-terms l2))))
                  (else
                    (adjoin-term (add-term t1 t2)
                                 (+terms (rest-terms l1)
                                         (rest-terms l2)))))))))

;;; DECENTRALIZED CONTROL

;;; -------------------------


;;; (set! <var> <value>)

(define (fact n)
  (let ((m 1) (i 1))
    (define (loop)
      (cond ((> i n) m)
            (else
              ;;; 注意这两行如果调换顺序, 就出错了
              (set! m (* m i))
              (set! i (+ i 1))
              (loop))))
    (loop)))

; (display-newline (fact 5))

;;; -----------------------
;;; Environment Model
;;; -----------------------

;;; Bound Variables
;;; ----------------
;;; We say that a variable V, is "bound in an expression" E.
;;; If the meaning of E is unchanged by the uniform replacement of
;;; a variable W (E 中没有这个变量),
;;; not occuring in E, for every occurrence of V in E.

;;; Free Varibales
;;; ----------------
;;; We say that a variable V, is "free in an expression" E.
;;; If the meaning of E is changed by the uniform replacement of
;;; a variable W, not occuring in E for every occurrence of V in e.

;;; Scope
;;; ----------------
;;; If x is a bound variable in E then there is a lambda expression
;;; where it is bound. We call the list of formal parameters of the
;;; lambda expression the "bound variable list" and we say that
;;; the lambda expression "binds" the variables "declared" in its bound
;;; variables list. In addition, these parts of expression where
;;; a variable has a value defined by the lambda expression
;;; which binds it is called the "scope" of the variable.

;;; 环境是一种结构化的表 -- 由框架组成.
;;; 框架是环境的一部分, 它们被链接在一起构成环境.
;;; 环境就是你寻找一个自由变量的值到底是多少的地方.

;;; 过程由两个部分组成,
;;; 一个部分指向代码,
;;; 一个部分指向寻找自由变量值的环境.

;;; 环境模型的两条规则 --- 太长了, 不写了

(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (+ n 1))
      n)))

; (define counter1 (make-counter 0))
; (define counter2 (make-counter 0))
; (display-newline (counter1))
; (display-newline (counter1))
; (display-newline (counter2))
; (display-newline (counter2))

;;; 有两个看起来一样的东西,
;;; 怎么判断他们是同一个东西还是两个不同的东西呢,
;;; 改变一个, 看另一个是是否也改变.

;;; Cesaro's method for estimating Pi:
;;;   Prob(gcd(n1, n2) = 1) = 6 / (Pi * Pi)

(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (cesaro)
  (= (gcd (random 1000000000) (random 100000000)) 1))

(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond ((= remaining 0)
           (/ passed trials))
          ((experiment)
           (iter (- remaining 1)
                 (+ passed 1)))
          (else
            (iter (- remaining 1)
                  passed))))
  (iter trials 0))

; (display-newline (estimate-pi 100000))

; (define rand
;   (let ((x random-init))
;     (lambda ()
;       (set! x (rand-update x))
;       x)))

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

(define (inverter in out)
  (define (invert-in)
    (let ((new
            (logical-not (get-signal in))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! out new)))))
  (add-action! in invert-in))

(define (logical-not a)
  (cond ((= a 0) 1)
        ((= a 1) 0)
        (else (error "invalid signal" a))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new
            (logical-and (get-signal a1)
                         (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output
                                  new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(define (make-wire)
  (let ((signal 0) (action-procs '()))
    (define (set-my-signal! new)
      (cond ((= signal new) 'done)
            (else
              (set! signal new)
              (call-each action-procs))))
    (define (accept-action-proc proc)
      (set! action-procs
        (cons proc action-procs))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!)
             set-my-signal!)
            ((eq? m 'add-action!)
             accept-action-proc)
            (else
              (error "Bad message" m))))
    dispatch))

(define (call-each procedures)
  (cond ((null? procedures) 'done)
        (else
          ((car procedures))
          (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

(define (after-delay delay action)
  (add-to-agenda!
    (+ delay (current-time the-agenda))
    action
    the-agenda))

(define (propagate)
  (cond ((empty-agenda? the-agenda)
         'done)
        (else
          ((first-item the-agenda))
          (remove-first-item! the-agenda)
          (propagate))))

;;; agenda -- 优先队列
;;; agenda 需要有头节点,
;;; 因为有 remove-first-item! 操作, 第一个节点会经常变化

; (define (make-agenda))
; (define (current-time agenda))
; (define (empty-agenda? agenda))
; (define (add-to-agenda! time action agenda))
; (define (first-item agenda))
; (define (remove-first-item! agenda))

;;; Queue

; (define (make-queue))
; (define (insert-queue! queue item))
; (define (delete-queue! queue))
; (define (front-queue queue))
; (define (empty-queue? queue))

;;; (set-car! <pair> <value>)
;;; (set-cdr! <pair> <value>)

; (define the-agenda (make-agenda))
; (define inverter-delay 2)
; (define and-gate-delay 3)
; (define or-gate-delay 5)
; (define input-1 (make-wire))
; (define input-2 (make-wire))
; (define sum (make-wire))
; (define carry (make-wire))
; (probe 'sum sum)
; (probe 'carry carry)
; (half-adder input-1 input-2 sum carry)
; (set-signal! input-1 1)
; (propagate)
; (set-signal! input-2 1)
; (propagate)

;;; Identity
;;; 引入赋值之后, 就出现了两个东西是不是同一个的问题.

(define a (cons 1 2))
(define b (cons a a))
;;; 这是 (car b) (cdr b) a 指向同一个东西
;;; they are sharing here...
;;; 这种身份共享很难控制, 是大型系统变得复杂和充满 bug 的一个原因.
(set-car! (car b) 3)
; (display-newline a)
; (display-newline b)

;;; cons car cdr 的另一个实现
(define (my-cons x y) (lambda (m) (m x y)))
(define (my-car z) (z (lambda (x y) x)))
(define (my-cdr z) (z (lambda (x y) y)))

; (display-newline (my-car (my-cons 1 2)))
;;; 代换模型
;;; (my-car (my-cons 1 2))
;;; (my-car (lambda (m) (m 1 2)))
;;; ((lambda (m) (m 1 2)) (lambda (x y) x))
;;; ((lambda (x y) x) 1 2)
;;; 1

; (display-newline (my-cdr (my-cons 1 2)))
;;; 代换模型
;;; (my-car (my-cons 1 2))
;;; (my-car (lambda (m) (m 1 2)))
;;; ((lambda (m) (m 1 2)) (lambda (x y) y))
;;; ((lambda (x y) y) 1 2)
;;; 2

;;; "Lambda Calculus" Mutable Data
(define (my-cons x y)
  ;;; 注意这里和 "对象实现" 用到的 dispatch 很像
  (lambda (m)
    (m x
       y
       (lambda (n) (set! x n))
       (lambda (n) (set! y n)))))
(define (my-car z) (z (lambda (x y sa sd) x)))
(define (my-cdr z) (z (lambda (x y sa sd) y)))
;;; 用 set! 实现 set-car! set-cdr!
(define (my-set-car! z x) (z (lambda (a d sa sd) (sa x))))
(define (my-set-cdr! z y) (z (lambda (a d sa sd) (sd y))))

; (define tt (my-cons 1 2))
; (display-newline (my-car tt))
; (my-set-car! tt 3)
; (display-newline (my-car tt))

;;; ASSIGNMENT AND STATES
;;; STATE, CHANGE, TIME
;;; IDENTITY, OBJECT, SHARING

;;; MODULAR SYSTEMS

;;; STREAM PROCESSING

;;; 计算一棵树中所有奇数的平方和

(define (sum-odd-squares tree)
  (define (leaf-node? tree) (atom? tree))
  (define (left-branch tree) (car tree))
  (define (right-branch tree) (cadr tree))
  (if (leaf-node? tree)
      (if (odd? tree)
          (square tree)
          0)
      (+ (sum-odd-squares
           (left-branch tree))
         (sum-odd-squares
           (right-branch tree)))))

; (define t '(1 ((4 (2 5)) (8 3))))
; (display-newline (sum-odd-squares t))

(define (odd-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (odd? f)
              (cons f (next (+ 1 k)))
              (next (+ 1 k))))))
  (next 1))

; (display-newline (odd-fibs 12))

;;; 从信号处理的角度拆解上面的两个过程
;;; 信号发生器 -- 信号过滤器 -- 信号处理 -- 信号整合
;;; enumerates leaves --> filter odd? --> map square --> acc + 0
;;; enumerates interval --> map fib --> filter odd? --> acc cons '()

;;; 用 stream 来作为传递的信号
;;; for and x and y,
;;; (head-stream (cons-stream x y)) == x
;;; (tail-stream (cons-stream x y)) == y

(define (cons-stream x y) (cons x y))
(define (head-stream s) (car s))
(define (tail-stream s) (cdr s))
(define the-empty-stream '())
(define (empty-stream? s) (null? s))

(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (proc (head-stream s))
                   (map-stream proc (tail-stream s)))))

(define (filter-stream predicate s)
  (cond ((empty-stream? s) the-empty-stream)
        ((predicate (head-stream s))
         (cons-stream (head-stream s)
                      (filter-stream predicate (tail-stream s))))
        (else (filter-stream predicate (tail-stream s)))))

(define (accumulate-stream op initial s)
  (if (empty-stream? s)
      initial
      (op (head-stream s)
          (accumulate-stream op initial (tail-stream s)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head-stream s1)
                   (append-streams (tail-stream s1)
                                   s2))))

(define (enumerate-tree tree)
  (define (leaf-node? tree) (or (null? tree) (atom? tree)))
  (define (left-branch tree) (car tree))
  (define (right-branch tree) (cadr tree))
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
        (enumerate-tree (left-branch tree))
        (enumerate-tree (right-branch tree)))))

(define (enumerate-intreval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-intreval (+ low 1) high))))

(define (sum-odd-squares tree)
  (accumulate-stream
    +
    0
    (map-stream square
                (filter-stream odd?  (enumerate-tree tree)))))

(define (odd-fibs low high)
  (accumulate-stream
    cons
    '()
    (filter-stream odd? (map-stream fib (enumerate-intreval low high)))))

; (define t '(1 ((4 (2 5)) (8 3))))
; (display-newline (sum-odd-squares t))
; (display-newline (odd-fibs 1 12))

(define (flatten-stream st-of-st)
  (accumulate-stream append-streams the-empty-stream st-of-st))

; (display-newline (flatten-stream '((1 2) (2 3) (2 5 7))))

(define (flatten-map-stream f s)
  (flatten-stream (map-stream f s)))

;;; given n find all pairs 0<j<i<=n
;;; such that i + j is prime

(define (prime? n)
  (define (iter g)
    (cond ((< n 2) #f)
          ((> (square g) n) #t)
          ((not (= (remainder n g) 0))
           (iter (+ g 1)))
          (else #f)))
  (iter 2))

(define (prime-sum-pairs n)
  (map
    (lambda (p)
      (list (car p) (cadr p) (+ (car p) (cadr p))))
    (filter
      (lambda (p) (prime? (+ (car p) (cadr p))))
      ;;; 区间映射为序对
      (flatten-map-stream
        (lambda (i)
          (map-stream (lambda (j) (list i j))
                      (enumerate-intreval 1 (- i 1))))
        (enumerate-intreval 1 n)))))

; (display-newline (prime-sum-pairs 6))

;;; collect 语法糖
; (define (prime-sum-pairs n)
;   (collect
;     (list i j (+ i j))
;     ((i (enum-interval 1 n))
;      (j (enum-interval 1 (- i 1))))
;     (prime? (+ i j))))

;;; 八皇后问题
;;; (safe? <row> <column> <rest-of-positions>)
;;; 回溯法 backtracking search -- 因为关注时间而过于复杂
;;; 递归策略 -- 假设 k-1 个已经摆放好了, 然后过滤出安全的第 k 个位置
;;; (define (queens size)
;;;   (define (fill-cols)
;;;     (if (= k 0)
;;;         (singleton empty-board)
;;;         (collect
;;;           (adjoin-position try-row
;;;                            k
;;;                            rest-queens)
;;;           ((rest-queens (fill-cols (- k 1)))
;;;            (try-row (enum-interval 1 size)))
;;;           (safe? try-row k rest-queens))))
;;;   (fill-cols size))

;;; (cons-stream x y)
;;; abbreviation for (cons x (delay y))
;;; (head s) --> (car s)
;;; (tail s) --> (force (cdr s))

;;; (head (tail (filter prime? (e-i 10000 1000000))))

;;; (delay <expr>)
;;; abbreviation for (lambda () (expr))

;;; (force p) = (p)

;;; 通过 delay 和 force 我们解耦了程序的逻辑顺序和实际的运行顺序

;;; 改进版 delay --> (memo-proc (lambda () (expr)))

;;; 为了避免 (tail (tail (tail ...)))
;;; 其实就是保存了每次 tail 的结果, 给下一次 tail 使用
(define (memo-proc proc)
  (let ((already-run? '())
        (result '()))
    (lambda ()
      (if (not already-run?)
          (sequence
            (set! result (proc))
            (set! already-run? (not '()))
            result)
          result))))

(define (cons-stream x y) (cons x y))
(define (head s) (car s))
;;; 这里没有用 force 而是直接调用求值
(define (tail s) ((cdr s)))

(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (- n 1) (tail s))))

(define (print-stream s)
  (define print display-newline)
  (cond ((empty-stream? s) "done")
        (else (print (head s))
              (print-stream (tail s)))))

(define (integers-from n)
  (cons-stream
    n
    ;;; 这里写成函数, 避免应用序导致无限循环 -- delay
    (lambda () (integers-from (+ n 1)))))

(define integers (integers-from  1))

(define (filter-stream predicate s)
  (define head-stream head)
  (define tail-stream tail)
  (cond ((empty-stream? s) the-empty-stream)
        ((predicate (head-stream s))
         (cons-stream (head-stream s)
                      (lambda ()
                        (filter-stream predicate (tail-stream s)))))
        (else (filter-stream predicate (tail-stream s)))))

; (display-newline (nth-stream 20 integers))

(define (no-seven? x) (not (= (remainder x 7) 0)))
(define ns (filter-stream no-seven? integers))

; (display-newline (nth-stream 100 ns))
; (print-stream ns)

;;; Eratosthenes 算法 -- 筛法求质数

(define (divisible? a b) (= (remainder a b) 0))
; (display-newline (divisible? 3 2))
; (display-newline (divisible? 232 2))

(define (sieve s)
  (cons-stream
    (head s)
    (lambda ()
      (sieve (filter-stream
               (lambda (x)
                 (not (divisible? x (head s))))
               (tail s))))))

(define primes (sieve (integers-from 2)))
; (display-newline (nth-stream 20 primes))
; (print-stream primes)

;;; -----  一个流实现  ------

; (define (cons-stream x f)
;   (cons x (lambda () (cons-stream (f x) f))))

; (define head car)

; (define (tail s) ((cdr s)))

; (define 1+ (lambda (x) (+ 1 x)))

; (define nats (cons-stream 0 1+))

; (define (nth-stream n s)
;   (display-newline (head s))
;   (if (= n 0)
;       (head s)
;       (nth-stream (tail s) (- n 1))))

; (display-newline (head (tail (tail nats))))
; (display-newline (nth-stream nats 3))

;;; ---------- 上面的实现对于 map ... 有问题 ------------

; (define (map-stream proc s)
;   (cons-stream (proc (head s))
;                (map-stream proc (tail s))))

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
          (cons-stream
            (+ (head s1) (head s2))
            (lambda () (add-streams (tail s1) (tail s2)))))))

(define (scale-stream c s)
  (map-stream (lambda (x) (* x c)) s))

(define ones (cons-stream 1 (lambda () ones)))

; (display-newline (nth-stream 12 ones))

(define nats (cons-stream 0 (lambda () (add-streams nats ones))))

; (display-newline (nth-stream 12 nats))
; (print-stream nats)

; (display-newline (nth-stream 12 (add-streams nats nats)))

(define (integral s initial-value dt)
  (define int
    (cons-stream
      initial-value
      (lambda () (add-streams (scale-stream dt s) int))))
  int)

(define fibs
  (cons-stream 0
               (lambda ()
                 (cons-stream 1
                              (lambda ()
                                (add-streams fibs (tail fibs)))))))

; (display-newline (nth-stream 1 fibs))
; (display-newline (nth-stream 2 fibs))
; (display-newline (nth-stream 3 fibs))
; (display-newline (nth-stream 4 fibs))
; (display-newline (nth-stream 5 fibs))
; (display-newline (nth-stream 6 fibs))
; (display-newline (nth-stream 7 fibs))

;;; 当两个流有依赖关系时, 需要手动延迟执行其中一个 .
;;; 当有多个流彼此依赖时, 就很难弄清楚改延迟执行哪一个.
;;; 解决办法就是把所有的过程都延迟执行.
;;; --------------------------------------------------
;;; normal-order evaluation language
;;; (我们之前一直用的 applicative-order evaluation)

;;; 正则序的缺点
;;; 1. 无法优先的表达迭代 -- 拖尾问题
;;; 2. 正则序和副作用是不相容的 (正则序和代换模型对应)

;;; Eval is a "Universal Machine"

(define my-eval
  (lambda (expr env)
    (cond
      ;;; *** special forms ***
      ;;; 3 -> 3
      ((number? expr) expr)
      ;;; x -> 3; car -> #[process]
      ((symbol? expr) (lookup expr env))
      ;;; 'foo --> (quote foo) -> foo
      ((eq? (car expr) 'quote) (cadr expr))
      ;;; (lambda (x) (+ x y)) --> (closure ((x) (+ x y)) <env>)
      ((eq? (car expr) 'lambda)
       (list 'closure (cdr expr) env))
       ;;; 不需要在定义时捕获外部环境
       ; expr)  ;;; 实现动态绑定 1.2
      ;;; (cond (p1 e1) (p2 e2) (p3 e3) ...)
      ((eq? (car expr) 'cond)
       (evcond (cdr expr) env))
      ;;; *** default combination ***
      (else
        ; (my-apply (my-eval (car env) env)
        ;           (evlist (cdr expr) env)
        ;           ;;; 调用时获取调用者的环境
        ;           ; env ;;; 实现动态绑定 1.2
        ;           )))))
        ;;; 特性 1.3 延迟求值
        ;;; (delay e) => (lambda () e)
        ;;; (force e) => (e)
        (my-apply (undelay (my-eval (car epxr)
                                    env))
                  (cdr expr)
                  env)))))

(define my-apply
  (lambda (proc args)
  ; (lambda (proc args) ;;; 实现动态绑定 1.2
    (cond ((primitive? proc)
           (apply-primop proc args))
          ((eq? (car proc) 'closure)
           ; ;;; proc = (LAMBDA bvrs body)
           ; (my-eval (cadadr proc)         ;;; body
           ;          (bind (caadr proc)    ;;; bvrs
           ;                args
           ;                (caddr proc))))
           ;                ; env))         ;;; env 实现动态绑定 1.2
           ;;; 特性 1.3 延迟求值
           ;;; proc = (CLOSURE (bvrs body) env)
           (my-eval (cadadr proc)                 ;;; body
                    (bind (vnames (caadr proc))
                          (gevlint (caadr proc)
                                   ops
                                   env)
                          (caddr proc))))         ;;; env
          (else "error"))))

(define evlist
  (lambda (l env)
    (cond ((eq? l '()) '())
          (else
            ; (cons (eval (car l) env)
            ;;; 特性 1.3 延迟求值
            (cons (undelay (eval (car l) env))
                  (evlist (cdr l) env))))))

(define gevlist
  (lambda (vars exps env)
    (cond
      ((eq? exps '()) '())
      ((symbol? (car vars)) ;;; applicative
       (cons (my-eval (car exps) env)
             (gevlist (cdr vars)
                      (cdr exps)
                      env)))
      ((eq? (caar vars) 'name)
       (cons (make-delay (car exps) env)
             (gevlist (cdr vars)
                      (cdr exps)
                      env)))
      (else error-unknown-declaration))))

(define evcond
  (lambda (clauses env)
    (cond ((eq? clauses '()) '())             ;;; arbitrary
          ((eq? (caar clauses) 'else)
           (my-eval (cadar clauses) env))
          ;;; 特性 1.3 延迟求值
          ((false? (undelay
                     (my-eval (caar clauses)
                              env)))
           (evcond (cdr clauses) env))
          ;;; true
          (else
            (my-eval (cadar clauses) env)))))

(define false? (lambda (x) (eq? x '())))

(define make-delay
  (lambda (expr env)
    (cons 'thunk (cons expr env))))

(define (undelay v)
  (cond ((pair? v)
         (cond ((eq? (car v) 'thunk)
                (undelay
                  (my-eval (cadr thunk)
                           (cddr thunk))))
               (else v)))
        (else v)))

(define bind
  (lambda (vars vals env)
    ;;; 把形参和实参配对, 环境就是一个框架表
    (cons (pair-up vars vals)
          env)))

(define pair-up
  (lambda (vars vals)
    (cond ((eq? vars '())
           (cond ((eq? vals '()) '())
                 (else (error TMA vars vals))))
          ;;; 特性 1.1
          ;;; 参数表的尾部不是 '() 而是一个 symbol, 比如 (x . y)
          ;;; (x . y) 是 (cons x y), 和 (list x y) 不同
          ;;; 那么就可以用这个 symbol 去匹配所有的值 (表)
          ((symbol? vars)
           (cons (cons vars vals) '()))
          ((eq? vals '()) (error TFA vars vals))
          (else
            (cons (cons (car vars)
                        (car vals))
                  (pair-up (cdr vars)
                           (cdr vals)))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env '()) (error UBV env))
          (else
            ((lambda (vcell)
               (cond ((eq? vcell '())
                      (lookup sym
                              (cdr env)))
                     (else (cdr vcell))))
             (my-assq sym (car env)))))))

(define my-assq
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
          ((eq? sym (caar alist))
           (car alist))
          (else
            (my-assq sym (cdr alist))))))

; Y = (lambda (f)
;       ((lambda (x) (f (x x)))
;        (lambda (x) (f (x x)))))

; (Y F) = (F (Y F))

;;; 实现一个语言特性 -- 允许过程有任意多个参数
;;; 首先要考虑一种语法表示方式
;;; (lambda (x . y)
;;;   ; u 表示点后面的参数
;;;   (map (lambda (u) (* x u))
;;;        y)
;;;   x required
;;;   many args y will be the list of them
;;; (lambda x x) = list
;;; 通过修改元循环器实现上面语法特性 (特性 1.1)

;;; 动态绑定 (特性 1.2)
;;; 实现上需要修改 eval 和 apply
;;; 缺点: 破话了模块性, 一个人代码中的名字会影响到另一个人

;;; 延迟求值 (特性 1.3)
;;; 为了通过 cond 实现 if (或 unless), 需要这样的语言特性
(define (my-unless p c a)
  (cond ((not p) c)
        (else a)))

; (display-newline (my-unless (= 1 0) 2 (/ 1 0)))
;;; 应用序 -- 代换模型, 就会报错

;;; 设计思路
;;; 首先得标记处 c 和 a 是特殊的, 不要在 apply 的时候就求值
;;; 然后考虑如何标记, 注意要避免已有语法产生歧义

; (define (my-unless p (name c) (name a))
;   (cond ((not p) c)
;         (else a)))

;;; 逻辑程序设计语言 (Prolog)
;;; 什么是真的, 检测它是不是真的, 找出那些真的

; (job (Bitdiddle Ben) (computer wizard))
; (salary (Bitdiddle Ben) 40000)
; (supervisor (Bitdiddle Ben) (Warbucks Oliver))
; (address (Bitdiddle Ben) (Slunerville (Ridge Road) 10))

; (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
; (job (Hacker Alyssa P) (compter programmer))
; (salary (Hacker Alyssa P) 35000)
; (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

; (address (Tweakit Lem E) (Boston (Bay State Road) 22))
; (job (Tweakit Lem E) (compter technician))
; (salary (Tweakit Lem E) 15000)
; (supervisor (Tweakit Lem E) (Bitdiddle Ben))

; (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
; (job (Reasoner Louis) (compter programmer trainee))
; (salary (Reasoner Louis) 20000)
; (supervisor (Reasoner Louis) (Hacker Alyssa P))

; (address (Warbucks Oliver) (Swellesley (The Manor)))
; (job (Warbucks Oliver) (administration big wheel))
; (salary (Warbucks Oliver) 100000)

;;; primitive -- query
;;; combination -- and, not, or, list-value
;;; abstraction -- rules

; (and (job ?x (computer . ?y))
;      (supervisor ?x ?z))
;;; List all people who work in the computer division,
;;; together with their supervisor.

; (and (salary ?p ?a)
;      (list-value > ?a 30000))
;;; List all people whose salary is greater than 30000.

; (and (job ?x (computer . ?y))
;      (not (and (supervisor ?x ?z)
;                (job ?z (computer . ?w)))))
;;; List all people who work in the computer division,
;;; who do not have a supervisor who works in the computer division.

; (rule
;   (bigshot ?x ?dept)                    ;;; rule conclustion
;   (and
;     (job ?x (?dept . ?y))
;     (not (and (supervisor ?x ?z)
;               (job ?z (?dept . ?w)))))) ;;; rule body

;; 没有 body 的 rule 总是真的


;;; -------------------------- TODO --------------------------------

(exit)

