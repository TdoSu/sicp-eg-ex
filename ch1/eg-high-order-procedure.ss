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

(exit)

