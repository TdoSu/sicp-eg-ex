(load "utils.ss")

;;; 采用 (var val) 的结构表示框架

(define (enclosing-enviroment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-enviroment '())

(define (make-frame variables values)
  (if (null? variables)
      '()
      (cons (cons (car variables) (car values))
            (make-frame (cdr variables) (cdr values)))))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))

;;; 重新实现上面 4 个过程之后, 下面的过程都不需要改写

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
    (scan (frame-variables)
          (frame-values frame))))


(exit)

