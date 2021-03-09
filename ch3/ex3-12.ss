(load "utils.ss")

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(display-newline z)
;;; (a b c d)

(display-newline (cdr x))
;;; (b)

(define w (append! x y))

(display-newline w)
;;; (a b c d)

(display-newline (cdr x))
;;; (b c d)

(exit)

