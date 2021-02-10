(load "utils.ss")

(define (fast-expt b n)
  (define (iter base counter result)
    (cond ((= counter 0) result)
          ((even? counter) (iter (square base) (/ counter 2) result))
          (else (iter base (- counter 1) (* base result)))))
  (iter b n 1))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (report-time f)
  (let ((start (runtime)))
    (begin (display (f))
           (display " - ")
           (display (- (runtime) start))
           (newline))))

(report-time (lambda () (expmod 1234567890 123456 7)))
;;; 需要 6928ms 左右

(define (expmod-fast base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(report-time (lambda () (expmod-fast 1234567890 123456 7)))
;;; 只需要 1747ms 左右

;;; 上面的方法计算出来的 (fast-expt base exp) 会是一个非常大的数,
;;; 而下面的方法一遍乘幂的同时, 一遍通过 remainder 缩小了数字, 避免了大数计算.

(exit)

