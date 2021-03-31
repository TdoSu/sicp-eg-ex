(load "utils.ss")

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
        (else
          (error 'argument "unknown expression type -- DERIV" expr))))

(define (operator expr) (car expr))
(define (operands expr) (cdr expr))

(define (make-exponentiation b e)
  (cond ((and (number? e) (= e 0)) 1)
        ((and (number? b) (= b 1)) 1)
        (else (list '** b e))))
(define (exponentiation? x) (and (pair? x) (eq? '** (car x))))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((or (and (number? m1) (= m1 0)) (and (number? m2) (= m2 0))) 0)
        ((and (number? m2) (= m2 1)) m1)
        ((and (number? m1) (= m1 1)) m2)
        (else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? '+ (car x))))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? '* (car x))))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;;; 用数据导向的风格改写符号求导

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

(define (deriv-exponentiation operands)
  (make-product (cadr operands)
                (make-product (make-exponentiation (car operands)
                                                   (- (cadr operands) 1))
                              (deriv (car operands) var))))

(put 'deriv '** deriv-exponentiation)

(display-newline (deriv '(+ x 3) 'x))
(display-newline (deriv '(* x y) 'x))
(display-newline (deriv '(* (* x y) (+ x 3)) 'x))

;;; 要改写为 ((get (operator expr) 'deriv) (operands expr) var)
;;; 只需要 put 的时候, 采用 (operator expr) 'deriv 这个顺序就可以了

(exit)

