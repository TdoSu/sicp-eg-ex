(load "utils.ss")

;;; 将 sum 和 product 抽象成 accumulate

;;; 递归
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;;; 迭代
(define (accumulate combiner null-value term a next b)
  (define (iter current result)
    (if (> current b)
        result
        (iter (next current) (combiner (term current) result))))
  (iter a null-value))

;;; 注意哦: combiner 第一个参数是 current 第二个是 result,
;;; 和 js 的 reduce 相反了, 不过无伤大雅.

(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

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

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product n)
  (if (even? n)
      (product (lambda (x) (/ (* x (+ x 2)) (square (+ x 1))))
               2.0
               (lambda (x) (+ x 2))
               n)
      (error 'argument-error "n 应该是一个偶数")))

(display-newline (factorial 6))

(display-newline (* 4 (pi-product 1000)))

(exit)

