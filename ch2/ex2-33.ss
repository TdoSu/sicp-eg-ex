(load "utils.ss")

(define (accumulate op initail items)
  (if (null? items)
      initail
      (op (car items)
          (accumulate op initail (cdr items)))))

;;; 用 accumulate 实现 map, filter, append, length

(define (map proc items)
  (accumulate (lambda (c r) (cons (proc c) r))
              '()
              items))

(define (filter predicate items)
  (accumulate (lambda (c r)
                (if (predicate c) (cons c r) r))
              '()
              items))

(define (append list1 list2)
  (accumulate cons list2 list1))

(define (length items)
  (accumulate (lambda (c r) (+ 1 r)) 0 items))

(define s (list 1 2 3 4 5))

(display-newline (map square s))
(display-newline (filter even? s))
(display-newline (append (list 1 2 3) (list 4 5 6)))
(display-newline (length (list 1 2 3 4 5)))

(exit)

