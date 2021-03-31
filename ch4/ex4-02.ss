(load "utils.ss")

(define (eval expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr nev))
        ((quoted? expr) (text-of-quotation expr))

        ;;; 提升过程应用子句的位置
        ;;; 问题是 application? 会把
        ;;; 赋值 (set! <var> <value>) 
        ;;; 定义 (define <var> <value>)
        ;;; 都当做过程应用
        ((application? expr)
         (apply (eval (operator expr) env)
                (list-of-values (operands expr) env)))

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
        (else
          (error 'EVAL "Unknown expression type" expr))))

(define (application? expr) (pair? expr))

;;; 要提前过程应用判断, 就不能用 pair? 来判断, 进而需要改写过程应用的语法形式
;;; (process-name <arg1> ... <argn>)
;;; 改成
;;; (call process-name <arg1> ... <argn>)

(define (application? expr) (tagged-list? expr 'call))

(define (operator expr) (cadr expr))

(define (operands expr) (cddr expr))

(exit)

