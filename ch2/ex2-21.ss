(load "utils.ss")

;;; square-list 两个实现

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(display-newline (square-list (list 1 2 3 4)))

(exit)

