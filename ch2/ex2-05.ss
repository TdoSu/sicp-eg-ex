(load "utils.ss")

;;; 用 2^a * 3^b 表示序对

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z) (count z 2))

(define (cdr z) (count z 3))

(define (count z n)
  (if (not (= (remainder z n) 0))
      0
      (+ 1 (count (/ z n) n))))

(display-newline (car (cons 3 4)))
(display-newline (cdr (cons 3 4)))

(exit)

