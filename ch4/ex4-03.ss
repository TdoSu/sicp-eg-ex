(load "utils.ss")

;;; 用数据导向的方式实现 eval

(define (eval expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        (else (if (not (null? (get 'evaluation (car expr))))
                  ((get 'evaluation (car expr))
                   expr
                   env)))
                  (erro 'EVAL "Unknown expression type" expr)))
  ;;; 前 3 种特殊情况
  ; (cond ((self-evaluating? expr) expr)
  ;       ((variable? expr) (lookup-variable-value expr nev))
  ;       ((quoted? expr) (text-of-quotation expr))
  ;;; 后面每种都对应一个 operator
  ;       ((assignment? expr) (eval-assignment expr env))
  ;       ((definition? expr) (eval-definition expr env))
  ;       ((if? expr) (eval-if expr env))
  ;       ((lambda? expr)
  ;        (make-procedure (lambda-parameters expr)
  ;                        (lambda-body expr)
  ;                        env))
  ;       ((begin? expr)
  ;        (eval-sequence (begin-action expr) env))
  ;       ((cond? expr) (eval (cond->if expr) env))
  ;       ((application? expr)
  ;        (apply (eval (operator expr) env)
  ;               (list-of-values (operands expr) env)))
        ; (else
        ;   (error 'EVAL "Unknown expression type" expr))))

(put 'evaluation 'set! eval-assignment)
(put 'evaluation 'define eval-definition)
(put 'evaluation 'lambda (lambda (expr env)
                           (make-procedure (lambda-parameters expr)
                                           (lambda-body expr)
                                           env)))
(put 'evaluation 'begin (lambda (expr env)
                          (eval-sequence (begin-action expr) env)))
(put 'evaluation 'cond (lambda (expr env)
                         (eval (cond->if expr) env)))
(put 'evaluation 'call (lambda (expr env)
                         (apply (eval (operator expr) env)
                                (list-of-values (operands expr) env))))

;;; 数据导向求导

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ;;; 把表达式分成连个部分 operator operands
        ;;; 根据 operator 和 'deriv 在表格中查找对应的过程进行处理
        ;;; number? variable? 不能这样写, 因为 number 和 variable? 没有操作符
        (else ((get 'deriv (operator expr))
               (operands expr)
               var))))

(define (deriv-sum operands)
  (make-sum (deriv (car operands) var)
            (deriv (cadr operands) var)))

(define (deriv-product operands)
  (make-sum (make-product (multiplier expr)
                          (deriv (multiplicand expr) var))
            (make-product (multiplicand expr)
                          (deriv (multiplier expr) var))))

(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)

(exit)

