(load "utils.ss")

;;; 添加 and 和 or 的实现

(define (eval expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr nev))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr)
         (make-procedure (lambda-parameters expr)
                         (lambda-body expr)
                         env))
        ((begin? expr)
         (eval-sequence (begin-action expr) env))
        ((cond? expr) (eval (cond->if expr) env))


        ;;; 直接实现 and 和 or
        ((and? expr)
         (eval-and expr env))
        ((or? expr)
         (eval-or expr env))

        ;;; 作为派生表达式
        ((and? expr)
         (eval (and->if expr) env))
        ((or? expr)
         (eval (or->if expr) env))

        ((application? expr)
         (apply (eval (operator expr) env)
                (list-of-values (operands expr) env)))
        (else
          (error 'EVAL "Unknown expression type" expr))))

(define (and? expr) (tagged-list? expr 'and))

;;; 参考 if
; (define (eval-if expr env)
;   (if (true? (eval (if-predicate expr) env))
;       (eval (if-consequent expr) env)
;       (eval (if-alternative expr) env)))

(define (eval-and expr env)
  (define (iter operands)
    (cond ((null? operands) 'true)
          ((not (true? (eval (car operands) env))) 'false)
          (else (iter (cdr operands)))))
  (iter (cdr expr)))

;;; 参考 cond
; (define (cond->if expr)
;   (expand-clauses (cond-clauses expr)))
; (define (expand-clauses clauses)
;   (if (null? clauses)
;       'false                      ; clause else no
;       (let ((first (car clauses))
;             (rest (cdr clauses)))
;         (if (cond-else-clause? first)
;             (if (null? rest)
;                 (sequence->exp (cond-actions first))
;                 (error 'COND->IF "ELSE clause isn't last" clauses))
;             (make-if (cond-predicate first)
;                      (sequence->exp (cond-actions first))
;                      (expand-clauses rest))))))

(define (and->if expr)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'true
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (make-if first
                   (expand-clauses rest)
                   'false))))
  (expand-clauses (cdr expr)))

(define (or? expr) (tagged-list? expr 'or))

(define (eval-or expr env)
  (define (iter operands)
    (cond ((null? operands) 'false)
          ((true? (eval (car operands) env)) 'true)
          (else (iter (cdr operands)))))
  (iter (cdr expr)))

(define (or->if expr)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (make-if first
                   'true
                   (expand-clauses rest)))))
  (expand-clauses (cdr expr)))

(exit)

