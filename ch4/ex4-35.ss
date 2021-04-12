(load "utils.ss")

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;;; 获取一个范围内的整数
(define (an-integer-between low high)
  (let (n (an-integer-starting-from low))
    (require (<= n high))
    n))

(define (a-pythagorean-triple-between low high)
  (let (i (an-integer-between low high))
    (let (j (an-integer-between i high))
      (let (k (an-integer-between j high))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(exit)

