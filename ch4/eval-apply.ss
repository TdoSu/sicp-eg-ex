(load "utils.ss")

(display-newline "--------- 元循环求值器 ---------")

(define true #t)
(define false #f)

;;; 求值器的内核

;;; 1. 对于自身求值表达式, 返回表达式本身 -- 数字
;;; 2. 环境中的变脸, 变量对应的值
;;; 3. 加引号的表达式, 返回被引用的表达式
;;; 4. 变量赋值和定义, 递归调用 eval 求出表达式的值, 然后修改环境
;;; 5. if 谓词为真, 求值推论部分, 否则求值另一个部分
;;; 6. lambda 表达式转化为一个可以应用的过程
;;;     方法是将 lambda 表达式描述的参数表和体 与 相应的求值环境包裹起来
;;; 7. begin 按顺序求值一些列表达式
;;; 8. cond 变换为一组 if
;;; 9. 过程应用, 递归求值运算符和运算对象, 然后把得到的过程和参数传递给 apply

(define (my-eval expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
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
        ((cond? expr)
         (my-eval (cond->if expr) env))
        ((application? expr)
         (my-apply (my-eval (operator expr) env)
                   (list-of-values (operands expr) env)))
        (else
          (error 'EVAL "Unknown expression type" expr))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error 'APPLY "Unknown procedure type" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if expr env)
  (if (true? (my-eval (if-predicate expr) env))
      (my-eval (if-consequent expr) env)
      (my-eval (if-alternative expr) env)))

(define (eval-sequence exps env)
  (cond ((last-expr? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (my-eval (assignment-value expr) env)
                       env)
  'ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (my-eval (definition-value expr) env)
                    env)
  'ok)


;;; 表达式的表示 -- 语法

(define (self-evaluating? expr)
  (cond ((number? expr) true)
        ((string? expr) true)
        (else false)))

(define (variable? expr)
  (symbol? expr))

(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (text-of-quotation expr) (cadr expr))

(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      false))

;;; 赋值 (set! <var> <value>)
(define (assignment? expr)
  (tagged-list? expr 'set!))

(define (assignment-variable expr) (cadr expr))
(define (assignment-value expr) (caddr expr))

;;; 定义 (define <var> <value>)

(define (definition? expr)
  (tagged-list? expr 'define))

(define (definition-variable expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      ;;; 兼容 (define (<var> <parameter1 ... <parametern>>) <body>)
      (caadr expr)))

(define (definition-value expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (make-lambda (cdadr expr)   ; formal parameters
                   (cddr expr)))) ; body

(define (lambda? expr) (tagged-list? expr 'lambda))

(define (lambda-parameters expr) (cadr expr))

(define (lambda-body expr) (cddr expr))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? expr) (tagged-list? expr 'if))

(define (if-predicate expr) (cadr expr))

(define (if-consequent expr) (caddr expr))

(define (if-alternative expr)
  (if (not (null? (cdddr expr)))
      (cadddr expr)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if consequent alternative))

(define (begin? expr)
  (tagged-list? expr 'begin))

(define (begin-actions expr) (cdr expr))

(define (last-expr? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-expr? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? expr) (pair? expr))

(define (operator expr) (car expr))

(define (operands expr) (cdr expr))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;; 派生表达式 用 if 实现 cond

(define (cond? expr) (tagged-list? expr 'cond))

(define (cond-clauses expr) (cdr expr))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

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
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;; 求值器的数据结构

;;; 谓词检测
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;;; 基本过程
; (apply-primitive-procedure <proc> <args>)
; (primitive-procedure? <proc>)

;;; 复合过程
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;; 环境操作
; (lookup-variable-value <var> <env>)
; (extend-environment <variables> <values> <baseenv>)
; (define-variable! <var> <value> <env>)
; (set-variable-value! <var> <value> <env>)

;;; 将环境实现为框架的表

(define (enclosing-enviroment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-enviroment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error 'EXTEND-ENVRIONMENT "Too many arguments supplied" vars vals)
          (error 'EXTEND-ENVRIONMENT "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             ;;; 空框架则查找外部环境
             (env-loop (enclosing-enviroment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    ;;; 空环境则报错
    (if (eq? env the-empty-enviroment)
        (error 'LOOKUP-VARIABLE-VALUE "Unbound variable" var)
        ;;; 取出第一个框架
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             ;;; 空框架则查找外部环境
             (env-loop (enclosing-enviroment env)))
            ((eq? var (car vars))
             ;;; 这里和 lookup-variable-value 不同 !
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    ;;; 空环境则报错
    (if (eq? env the-empty-enviroment)
        (error 'LOOKUP-VARIABLE-VALUE "Unbound variable" var)
        ;;; 取出第一个框架
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;; 基本过程

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        ;;; <其他基本过程>
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-enviroment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  ; (apply-in-underlying-scheme
  (apply
    (primitive-implementation proc) args))

;;; 驱动循环

(define input-prompt ";;; M-Eval input: ")
(define output-prompt ";;; M-Eval value: ")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(driver-loop)

(exit)

