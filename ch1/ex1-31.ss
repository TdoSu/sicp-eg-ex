(load "utils.ss")

;;; 模仿 sum  抽象 product

;;; 递归
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

;;; 迭代
(define (sum term a next b)
  (define (iter current result)
    (if (> current b)
        result
        (iter (next current) (* (term current) result))))
  (iter a 1))

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
;;; 这个收敛也挺慢的

(exit)

