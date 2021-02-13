(load "utils.ss")

;;; deep-reverse 深度翻转一颗树

(define (deep-reverse x)
  (cond ((null? x) '())
        ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x))
                      (list (deep-reverse (car x)))))))

(define x (list (list 1 2) (list 3 4)))

(display-newline x)
(display-newline (reverse x))
(display-newline (deep-reverse x))

(display-newline (deep-reverse '(1 2 (3 4))))
(display-newline (deep-reverse '((-1 0 (0.5)) 1 2 (3 4))))

(exit)

