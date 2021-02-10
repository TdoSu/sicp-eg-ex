(load "utils.ss")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;; 写一个 search-for-prime 过程检测给定范围连续奇数的素性

(define (search-for-prime start end)
  (define (iter guess)
    (if (not (> guess end))
        (begin (timed-prime-test guess)
               (iter (+ 2 guess)))))
  (iter (if (odd? start) start (+ 1 start))))

;;; 找出大于 x 的 n 个最小的素数

(define (search-for-n-primes x n)
  (cond ((= n 0) "DONE")
        ((fast-prime? x 1000)
         (begin (timed-prime-test x)
                (search-for-n-primes (+ x 1) (- n 1))))
        (else
          (search-for-n-primes (+ x 1) n))))

(search-for-n-primes (* 100000000 1000) 3)
(search-for-n-primes (* 100000000 10000) 3)
(search-for-n-primes (* 100000000 100000) 3)
(search-for-n-primes (* 100000000 1000000) 3)

;;; 使用 prime? 的时间
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

;;; 使用 fast-prime? 次数 1000 的时间
; 100000000003 *** 8
; 100000000019 *** 9
; 100000000057 *** 9
; 1000000000039 *** 8
; 1000000000061 *** 9
; 1000000000063 *** 9
; 10000000000037 *** 9
; 10000000000051 *** 9
; 10000000000099 *** 9
; 100000000000031 *** 11
; 100000000000067 *** 10
; 100000000000097 *** 10
;;; 时间几乎没有增长

(search-for-n-primes (* 1000000 100000000 1000) 3)
(search-for-n-primes (* 1000000 100000000 10000) 3)
(search-for-n-primes (* 1000000 100000000 100000) 3)
(search-for-n-primes (* 1000000 100000000 1000000) 3)

(newline)
(exit)

