(load "utils.ss")

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
    (scan (frame-variables)
          (frame-values frame))))

;;; 抽象上面 3 个过程

(define (lookup-variable-in-env var proc-empty-frame proc-target-var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             ;;; lookup-variable-value, set-variable-value!
             ;;; --> (lambda (env) (env-loop (enclosing-enviroment env)))
             ;;; define-variable!
             ;;; --> (lambda (env) (add-binding-to-frame! var val (first-frame env)))
             (proc-empty-frame env))
            ((eq? var (car vars))
             ;;; lookup-variable-value --> (lambda (vals) (car vals))
             ;;; set-variable-value! --> (set-car! vals val)
             (proc-target-var vals))
            (else (scan (cdr vars) (cdr vals)))))
    ;;; 空环境则报错
    (if (eq? env the-empty-enviroment)
        (error 'LOOKUP-VARIABLE-VALUE "Unbound variable" var)
        ;;; 取出第一个框架
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(exit)

