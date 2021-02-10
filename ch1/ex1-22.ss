(load "utils.ss")

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
        ((prime? x)
         (begin (timed-prime-test x)
                (search-for-n-primes (+ x 1) (- n 1))))
        (else
          (search-for-n-primes (+ x 1) n))))

(search-for-n-primes 1000 3)
(search-for-n-primes 10000 3)
(search-for-n-primes 100000 3)
(search-for-n-primes 1000000 3)

; 1009 *** 0
; 1013 *** 0
; 1019 *** 0
; 10007 *** 0
; 10009 *** 0
; 10037 *** 0
; 100003 *** 0
; 100019 *** 0
; 100043 *** 0
; 1000003 *** 0
; 1000033 *** 0
; 1000037 *** 0

;;; 心塞... 用时都不到 1/1000 秒, chez scheme 和现在机器的性能...

(search-for-n-primes (* 100000000 1000) 3)
(search-for-n-primes (* 100000000 10000) 3)
(search-for-n-primes (* 100000000 100000) 3)
(search-for-n-primes (* 100000000 1000000) 3)

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

;;; 提升 10 倍花费的时间大概是之前的 3 倍
;;; 接近 sqrt(10)

(newline )
(exit)

