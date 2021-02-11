(load "utils.ss")

;;; 组合利用 fixed-point, average-damp, repeated 求方根

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (expt x n)
  ((repeated (lambda (y) (* y x)) n) 1))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;;; (try n次方根 平均阻尼次数)
(define (try n damp-times)
  ((lambda (x)
    (fixed-point ((repeated average-damp damp-times)
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))
   (expt 3 n)))

(display-newline (try 2 1))
(display-newline (try 3 1))
(display-newline (try 4 2))
(display-newline (try 5 2))
(display-newline (try 6 2))
(display-newline (try 7 2))
(display-newline (try 8 3))
(display-newline (try 9 3))
(display-newline (try 10 3))
(display-newline (try 11 3))
(display-newline (try 12 3))
(display-newline (try 13 3))
(display-newline (try 14 3))
(display-newline (try 15 3))
(display-newline (try 16 4))

;;; 如果 n 小于 2 的最小 m 次幂, 那么 m - 1 就是需要使用平均阻尼的次数
(define (root-n x n)
  (define (damp-times n)
    (define (iter counter)
      (if (> (expt 2 counter) n)
          counter
          (iter (+ counter 1))))
    (iter 1))
  (fixed-point ((repeated average-damp (damp-times n))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(display-newline (root-n (expt 2 10) 10))
(display-newline (root-n (expt 2 100) 100))
(display-newline (root-n (expt 3 10) 10))
(display-newline (root-n (expt 3 99) 99))

(exit)

