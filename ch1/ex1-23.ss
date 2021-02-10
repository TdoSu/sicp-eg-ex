(load "utils.ss")

;;; 优化 smallest-divisor 跳过对偶数的检查

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (let ((next (if (even? test-divisor) (+ 1 test-divisor) (+ 2 test-divisor))))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n next)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (and (> n 1) (= (smallest-divisor n) n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-n-primes x n)
  (cond ((= n 0) "DONE")
        ((prime? x)
         (begin (timed-prime-test x)
                (search-for-n-primes (+ x 1) (- n 1))))
        (else
          (search-for-n-primes (+ x 1) n))))

(search-for-n-primes (* 100000000 1000) 3)
(search-for-n-primes (* 100000000 10000) 3)
(search-for-n-primes (* 100000000 100000) 3)
(search-for-n-primes (* 100000000 1000000) 3)

;;; 优化前
; 100000000003 *** 5
; 100000000019 *** 4
; 100000000057 *** 4
; 1000000000039 *** 15
; 1000000000061 *** 15
; 1000000000063 *** 15
; 10000000000037 *** 47
; 10000000000051 *** 47
; 10000000000099 *** 46
; 100000000000031 *** 145
; 100000000000067 *** 148
; 100000000000097 *** 152

;;; 优化后
; 100000000003 *** 3
; 100000000019 *** 3
; 100000000057 *** 3
; 1000000000039 *** 9
; 1000000000061 *** 8
; 1000000000063 *** 9
; 10000000000037 *** 27
; 10000000000051 *** 26
; 10000000000099 *** 27
; 100000000000031 *** 83
; 100000000000067 *** 82
; 100000000000097 *** 82

;;; 优化的时间不到 2 倍, 可以想象毕竟判断奇数偶数等操作也需要花费时间

(newline)
(exit)

