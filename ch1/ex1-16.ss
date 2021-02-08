(load "utils.ss")

;;; 快速幂的迭代实现

(define (fast-expt b n)
  (define (iter base counter result)
    (cond ((= counter 0) result)
          ((even? counter) (iter (square base) (/ counter 2) result))
          (else (iter base (- counter 1) (* base result)))))
  (iter b n 1))

(display-newline (fast-expt 2 10))
(display-newline (fast-expt 2 20))

(exit)

