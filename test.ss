(define (display-newline . msgs)
  (if (null? msgs)
      (newline)
      (begin (display (car msgs))
             (display " ")
             (apply display-newline (cdr msgs)))))

;;; -------------------------- TODO --------------------------------

(exit)

