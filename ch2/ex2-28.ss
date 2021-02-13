(load "utils.ss")

;;; 展开一棵树

(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

(define x (list (list 1 2) (list 3 4)))

(display-newline (fringe x))
(display-newline (fringe (list x x)))

(exit)

