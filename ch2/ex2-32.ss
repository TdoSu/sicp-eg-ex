(load "utils.ss")

;;; 求集合所有子集

(define (subsets s)
  (if (null? s)
      (list '())
      ;;; rest 不包含第一个元素的集合的所有子集
      (let ((rest (subsets (cdr s))))
        (append rest
                ;;; 包含第一个元素的所有子集
                (map (lambda (a) (cons (car s) a))
                     rest)))))

(display-newline (subsets '(1 2 3)))

(exit)

