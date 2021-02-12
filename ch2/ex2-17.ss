(load "utils.ss")

;;; last-pair 返回包含给定表最后一个元素的表

(define (last-pair items)
  (cond ((null? items) (error 'arguments "给定的表不能是空表"))
        ((null? (cdr items)) (list (car items)))
        (else (last-pair (cdr items)))))

(display-newline (last-pair (list 23 72 149 34)))
; (display-newline (last-pair '()))

(exit)

