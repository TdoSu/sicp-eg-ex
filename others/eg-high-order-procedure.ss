(load "utils.ss")

;;; 高阶过程
;;; 过程作为过程的参数, 过程作为过程的返回值

;;; 整数求和
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(display-newline (sum-integers 1 100))

;;; 立方数求和
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(display-newline (sum-cubes 1 3))

;;; 通过求数列和计算 pi
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(display-newline (* 8 (pi-sum 1 10000)))
;;; 这个收敛速度真的有够慢

;;; 上面三个过程有一个公共的概念 -- 求和
;;; 把它抽象出来 (抽取通用部分, 变化部分用参数代替)
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-integers a b)
  (sum (lambda (x) x) a (lambda (x) (+ x 1)) b))

(define (sum-cubes a b)
  (sum cube a (lambda (x) (+ x 1)) b))

(define (pi-sum a b)
  (sum (lambda (a) (/ 1.0 (* a (+ a 2))))
       a
       (lambda (x) (+ x 4))
       b))

(display-newline (sum-integers 1 100))
(display-newline (sum-cubes 1 3))
(display-newline (* 8 (pi-sum 1 10000)))

;;; 求积分
(define (integral f a b dx)
  (* (sum (lambda (x) (f (+ x (/ dx 2)))) a (lambda (x) (+ x dx)) b)
     dx))

(display-newline (integral cube 0 1 0.01))
;;; 近似 1/4

;;; 为了方便把过程作为一个值使用 -- 引入 lambda 表达式
;;; 或者为了方便命名一个过程 -- (define (name arguments) body)

;;; lambda 是一个特殊的过程
;;; 它产生一个过程, 接收两个参数, 第一个是参数列表, 第二个是过程体,
;;; 过程体中的变量收到参数列表约束.

;;; 用 lambda 表达式可以实现 let 局部命名

(let ((x 2) (y 3))
  (+ x y))
; 5

;;; 等价于

((lambda (x y)
   (+ x y))
 2
 3)

;;; 过程抽象 -- 让操作不依赖于具体的操作对象, 也不依赖于具体实现
;;; (* 3 3 3) (* 4 4 4) (* 3.14 3.14 3.14)
;;; 抽象成 (lambda (x) (* x x x)), x 可以指代任何数值.
;;; 另一个方面 (cube 3) (cube 4) (cube 5)
;;; (define cube (lambda (x (* x x x))))
;;; (define cube (lambda (x (* (square x) x))))
;;; (define cube (lambda (x (expt x 3))))
;;; 不依赖与 cube 具体怎么写, 将怎么实现和怎么用分离开!

;;; 通过区间折半查找, 找方程的根
(define (search f neg-point pos-point)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else (error 'arguments "Values are not of opposite sign" a b)))))

(display-newline (half-interval-method sin 2.0 4.0))

(display-newline (half-interval-method
                   (lambda (x) (- (cube x) (* 2 x) 3))
                   1.0
                   2.0))

;;; 寻找函数不动点
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display-newline (fixed-point cos 1.0))

(display-newline (fixed-point (lambda (y) (+ (sin y) (cos y)))
                              1.0))

;;; 用不定点实现求平方根
;;; 直接计算 (lambda (y) (/ x y)) 是不收敛的
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(display-newline (sqrt (square 3.0)))

;;; 过程作为返回值

;;; 平均阻尼

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(display-newline (cube-root (cube 3.0)))

;;; 牛顿法
;;; 如果 x -> g(x) 可微
;;; 那么 g(x) = 0 的一个解就是 x -> f(x) 的一个不动点
;;; 其中 f(x) = x - (g(x) / Dg(x))

;;; 先搞个求导函数
(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx))))

(display-newline ((deriv cube) 5))

;;; 牛顿法表述 g -> f
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

;;; 用牛顿法描述求方程的根
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

;;; 用牛顿法实现求 sqrt
(define (sqrt x)
  (fixed-point (newton-transform (lambda (y) (- x (square y)))) 1.0))

(display-newline (sqrt (square 3)))

;;; 更一般的求不动点过程

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;;; 过程作为第一级公民
;;; 1. 可以被命名
;;; 2. 可以作为参数传递给过程
;;; 3. 可以作为过程的返回值
;;; 4. 可以作为其他数据结构的一部分

(exit)

