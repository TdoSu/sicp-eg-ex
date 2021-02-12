(define (div-interval x y)
  (if (0-in-interval? y)
      (error 'arguments "除区间不应该跨越 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upp-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (0-in-interval? i)
  (and (<= (lower-bound i) 0)
       (>= (upper-bound i) 0)))

