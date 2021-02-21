(load "utils.ss")

(define (debug msg . args)
  (begin (apply display-newline (cons msg args))
         (error 'debug "------")))

;;; 修改求导规则 - 支持中缀表达式

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum (make-product (multiplier expr)
                                 (deriv (multiplicand expr) var))
                   (make-product (multiplicand expr)
                                 (deriv (multiplier expr) var))))
        ((exponentiation? expr)
         (make-product (exponent expr)
                       (make-product (make-exponentiation (base expr)
                                                          (- (exponent expr) 1))
                                     (deriv (base expr) var))))
        (else
          (error 'argument "unknown expression type -- DERIV" expr))))

;;; a. 所有的表达式都是二元的, 只是运算符在中间
;;;    这时表达式仍然是只有三个对象, 只是顺序变了,
;;;    调整构造函数, 选择函数, 谓词对应值的顺序就可以了

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        (else (list a1 '+ a2))))
(define (sum? x) (and (pair? x) (eq? '+ (cadr x))))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((or (and (number? m1) (= m1 0)) (and (number? m2) (= m2 0))) 0)
        ((and (number? m2) (= m2 1)) m1)
        ((and (number? m1) (= m1 1)) m2)
        (else (list m1 '* m2))))
(define (product? x) (and (pair? x) (eq? '* (cadr x))))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(display-newline (deriv '(x + (3 * (x + (y + 2)))) 'x))

;;; b. 支持数学形式的表达式, 可能不写括号, * 优先于 + (可以用到 memq 了)
;;;    首先判断是否有 +, 如果有就是和, 不是和在判断是不是积  -- 实现优先级
;;;    然后根据运算符切分两个参数

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        (else (list a1 '+ a2))))
(define (sum? x) (and (pair? x) (not (not (memq '+ x)))))
(define (addend s) (first-arg '+ s))
(define (augend s) (second-arg '+ s))

(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((or (and (number? m1) (= m1 0)) (and (number? m2) (= m2 0))) 0)
        ((and (number? m2) (= m2 1)) m1)
        ((and (number? m1) (= m1 1)) m2)
        (else (list m1 '* m2))))
(define (product? x)
  (and (pair? x)
       ;;; 实现乘法优先于加法
       (not (sum? x))
       (not (not (memq '* x)))))
(define (multiplier p) (first-arg '* p))
(define (multiplicand p) (second-arg '* p))

(define (first-arg operator s)
  (define (before-operator s)
    (cond ((or (not (pair? s)) (not (memq operator s))) s)
          ((eq? operator (car s)) '())
          (else (cons (car s) (before-operator (cdr s))))))
  (let ((first (before-operator s)))
    (if (null? (cdr first))
        (car first)
        first)))
(define (second-arg operator s)
  (let ((rest (cdr (memq operator s))))
    (if (null? (cdr rest))
        (car rest)
        rest)))

(display-newline (deriv '(x + y + 2) 'x))
(display-newline (deriv '(x * y * 2) 'x))
(display-newline (deriv '(x + 3 * (x + y + 2)) 'x))

(exit)

