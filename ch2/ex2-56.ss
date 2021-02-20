(load "utils.ss")

;;; 扩充符号求导, 实现求幂的导数

;;; 描述求导规则
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

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        (else (list '+ a1 a2))))
(define (sum? x) (and (pair? x) (eq? '+ (car x))))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((or (and (number? m1) (= m1 0)) (and (number? m2) (= m2 0))) 0)
        ((and (number? m2) (= m2 1)) m1)
        ((and (number? m1) (= m1 1)) m2)
        (else (list '* m1 m2))))
(define (product? x) (and (pair? x) (eq? '* (car x))))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-exponentiation b e)
  (cond ((and (number? e) (= e 0)) 1)
        ((and (number? b) (= b 1)) 1)
        (else (list '** b e))))
(define (exponentiation? x) (and (pair? x) (eq? '** (car x))))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(display-newline (deriv '(* 3 (** x 5)) 'x))

(exit)

