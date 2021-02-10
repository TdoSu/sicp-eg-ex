(load "utils.ss")

;;; 扩展 accumulate 成 filtered-accumulate

;;; 递归
(define (filtered-accumulate predicate combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((predicate (term a))
         (combiner (term a)
                   (filtered-accumulate predicate
                                        combiner
                                        null-value
                                        term
                                        (next a)
                                        next
                                        b)))
         
        (else (filtered-accumulate predicate
                                   combiner
                                   null-value
                                   term
                                   (next a)
                                   next
                                   b))))

;;; 迭代
(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter current result)
    (cond ((> current b) result)
          ((predicate (term current))
           (iter (next current) (combiner (term current) result)))
          (else (iter (next current) result))))
  (iter a null-value))

(define (sum-prime a b)
  (filtered-accumulate prime?  + 0 identity a inc b))

(display-newline (sum-prime 1 10))
; 2 + 3 + 5 + 7

(define (product-prime-n n)
  (filtered-accumulate (lambda (k) (= 1 (gcd k n)))
                       *
                       1
                       identity
                       1
                       inc
                       n))

(display-newline (product-prime-n 10))
; 3 * 7 * 9

(exit)

