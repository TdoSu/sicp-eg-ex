(load "utils.ss")

;;; 递归计算帕斯卡三角形

;;; 第 m 行 第 n 个
(define (pascal-number m n)
  (cond ((or (< m 1) (< n 1) (> n m)) 0)
        ((and (= m 1) (= n 1)) 1)
        (else
          (+ (pascal-number (- m 1) (- n 1))
             (pascal-number (- m 1) n)))))

(define (print-pascal-line m)
  (define (loop counter)
    (if (> counter m)
        (newline)
        (begin (display (pascal-number m counter))
               (display " ")
               (loop (+ counter 1)))))
  (loop 1))

(define (print-pascal-triangle m)
  (define (loop counter)
    (if (<= counter m)
        (begin (print-pascal-line counter)
               (loop (+ counter 1)))))
  (loop 1))

(print-pascal-triangle 5)

(exit)

