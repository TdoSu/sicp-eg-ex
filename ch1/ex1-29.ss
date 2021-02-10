(load "utils.ss")

;;; 辛普森规则求积分

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (if (even? n)
      (let ((h (/ (- b a) n)))
        (define (y k) (f (+ a (* k h))))
        (* (sum (lambda (k)
                  (* (cond ((or (= k 0) (= k n)) 1)
                           ((odd? k) 4)
                           ((even? k) 2))
                     (y k)))
                0
                (lambda (k) (+ k 1))
                n)
           (/ h 3.0)))
      (error 'argument-error "n 应该是一个偶数")))

(display-newline (simpson-integral cube 0 1 100))
;;; > 0.25 非常精准啊

(exit)

