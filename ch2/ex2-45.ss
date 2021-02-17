(load "utils.ss")

;;; 从 right-split up-split 抽象 split

(define (split t1 t2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
          (t1 painter (t2 smaller smaller)))))

(define right-split (split beside below))
(define up-split (split below beside))

(exit)

