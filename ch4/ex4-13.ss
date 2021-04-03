(load "utils.ss")

(display-newline "Hello, scheme!")

;;; 尝试实现 make-unbound! 删除约束

(define (eval expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))

        ;;; make-unbound! 是 define 的相反过程
        ((unbound? expr)
         (eval-make-unbound! expr env))

        ((if? expr) (eval-if expr env))
        ((lambda? expr)
         (make-procedure (lambda-parameters expr)
                         (lambda-body expr)
                         env))
        ((begin? expr)
         (eval-sequence (begin-action expr) env))
        ((cond? expr) (eval (cond->if expr) env))
        ((application? expr)
         (apply (eval (operator expr) env)
                (list-of-values (operands expr) env)))
        (else
          (error 'EVAL "Unknown expression type" expr))))

(define (make-unbound! var)
  (list 'unbound! var))

(define (unbound-variable expr)
  (cadr expr))

(define (unbound? expr)
  (tagged-list? 'unbound! expr))

;;; 应该接触当前环境使用的变量定义的绑定
;;; 参考 set-variable-value!
(define (eval-make-unbound! expr env)
  (let ((var (unbound-variable expr)))
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-enviroment env)))
              ;;; 注意这里应该拿到前一个 list 这样才能用 set-cdr! 搞定
              ((eq? var (cadr vars))
               (begin (set-cdr! vars (cddr vars))
                      (set-cdr! vals (cddr vals))))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-enviroment)
          (error 'EVAL-MAKE-UNBOUND! "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env)))

(exit)

