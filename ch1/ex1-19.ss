(load "utils.ss")

;;; -------------------
;;; 求斐波那契数的方法
;;; -------------------

;;; 递归方法 时间复杂度 O(phi^n) 空间复杂度 O(n)
(define (fib n)
  (cond ((< n 2) n)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

;;; 迭代写法 时间复杂度 O(n) 空间复杂度 O(1)
(define (fib n)
  (define (iter a b counter)
    (if (= counter n)
        a
        (iter  b (+ a b) (+ counter 1))))
  (iter 0 1 0))

;;; 求斐波那契数 O(log(n)) 算法
;;; T 变换
;;; a <-- a + b
;;; b <-- a
;;; T 变换反复应用于 (1, 0) n 次产生斐波那契数
;;; T 可以看做 Tpq 中 p = 0, q = 1 的特殊情况
;;; Tpq
;;; a <-- bq + aq + ap
;;; b <-- bp + aq

;;; 应用两次 Tpq
;;; a <-- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;;   <-- bpq + aqq + bqq + aqq + apq + bqp + aqp + app
;;;   <-- b(pq + qq + qp) + a(qq + qq + pq + qp + pp)
;;;   <-- b(qq + 2pq) + a(qq + 2pq + qq + pp)
;;;   <-- b(qq + 2pq) + a(qq + 2pq) + a(pp + qq)
;;; b <-- (bp + aq)p + (bq + aq + ap)q
;;;   <-- bpp + aqp + bqq + aqq + apq
;;;   <-- b(pp + qq) + a(qq + 2pq)
;;; 所以
;;; a <-- bq' + aq' + ap'
;;; b <-- bp' + aq'
;;; 其中 p' = pp + qq, q' = qq + 2pq

(define (fib n)
  (define (iter a b p q counter)
    (cond ((= counter 0) b)
          ((even? counter)
           (iter a
                 b
                 (+ (* p p) (* q q))
                 (+ (* q q) (* 2 p q))
                 (/ counter 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- counter 1)))))
  (iter 1 0 0 1 n))

;;; 利用通项公式 时间复杂度 O(1) 空间复杂度 O(1)


(display-newline (fib 0))
(display-newline (fib 1))
(display-newline (fib 2))
(display-newline (fib 3))
(display-newline (fib 4))
(display-newline (fib 5))
(display-newline (fib 6))

(exit)

