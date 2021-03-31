(load "utils.ss")

(display-newline
  (cond ((assoc 'b '((a 1) (b 2))) => cadr)
        (else #f)))

;;; 扩展 cond 的实现

(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                      ; clause else no
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error 'COND->IF "ELSE clause isn't last" clauses))
            (if (and (eq? (cadr clauses) '=>)
                     (eval (car clauses) env))
                (apply (caddr clauses)
                       (eval (car clauses) env))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(exit)

