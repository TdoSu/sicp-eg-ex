(load "utils.ss")

;;; 记录调用次数

(define (make-monitored f)
  (let ((count 0))
    (lambda (v)
      (cond ((eq? v 'how-many-calls?) count)
            ((eq? v 'reset-count) (begin (set! count 0) count))
            (else (begin (set! count (+ count 1)) (f v)))))))

(define s (make-monitored sqrt))

(display-newline (s 100))
(display-newline (s 'how-many-calls?))
(display-newline (s 20))
(display-newline (s 'how-many-calls?))
(display-newline (s 'reset-count))
(display-newline (s 'how-many-calls?))
(display-newline (s 20))
(display-newline (s 100))
(display-newline (s 'how-many-calls?))

(exit)

