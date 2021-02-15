(load "utils.ss")

; 抽象 unique-pairs 简化 prime-sum-pairs 过程

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map (lambda (p) (list (car p) (cadr p) (+ (car p) (cadr p))))
       (filter (lambda (p) (prime? (+ (car p) (cadr p))))
               (unique-pairs n))))

(display-newline (prime-sum-pairs 6))

(exit)

