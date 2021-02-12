(load "utils.ss")

(define (reverse items)
  (cond ((null? items) '())
        (else (append (reverse (cdr items))
                      (list (car items))))))

(define (reverse items)
  (define (iter count result a)
    (cond ((= count (length items)) result)
          (else (iter (+ 1 count)
                      (cons (car a) result)
                      (cdr a)))))
  (iter 0 '() items))

(display-newline (reverse (list 1 4 9 16 25)))

(exit)

