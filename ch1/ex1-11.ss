(load "utils.ss")

;;; x < 3 时, f(n) = n
;;; x >= 3 时, f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3)

;;; 递归
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;;; 迭代
(define (f n)
  (define (iter a b c counter)
    (if (= counter n)
        a
        (iter b
              c
              (+ c (* 2 b) (* 3 a))
              (+ counter 1))))
  (iter 0 1 2 0))

(display-newline (f 0))
(display-newline (f 1))
(display-newline (f 2))
(display-newline (f 3))
(display-newline (f 4))
(display-newline (f 5))
(display-newline (f 6))
(display-newline (f 7))

(exit)

