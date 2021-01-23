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



;;; -------------------------- TODO --------------------------------

(exit)

