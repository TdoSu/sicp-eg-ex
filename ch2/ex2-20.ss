(load "utils.ss")

;;; 返回与第一个参数奇偶性相同的表

(define (same-partity first . others)
  (define (iter result items)
    (cond ((null? items) result)
          ((even? (+ first (car items)))
           (iter (append result (list (car items))) (cdr items)))
          (else (iter result (cdr items)))))
  (iter (list first) others))

(display-newline (same-partity 1 2 3 4 5 6 7))
(display-newline (same-partity 2 3 4 5 6 7))

(exit)

