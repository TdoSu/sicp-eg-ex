(load "utils.ss")

;;; 符号数据

;;; 过程是一种延迟求值 (计算) 的手段
;;; 引号也是一种延迟求值的手段

;;; 加了引号之后 (+ 1 2) 并不会被求值
(define three '(+ 1 2))

;;; 至到作为 eval 参数才会被求值
(display-newline (eval three))

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(display-newline (memq 'apple '(pear banana prune)))
(display-newline (memq 'apple '(x (apple sauce) y apple pear)))


;;; 符号求导

;;; 设计基本过程
; (variable? e)
; (same-variable? v1 v2)
; (sum? e)
; (addend e)
; (augend e)
; (make-sum a1 a2)
; (product? e)
; (multiplier e)
; (multiplicand e)
; (make-product m1 m2)
; (number? x)

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
        (else
          (error 'argument "unknown expression type -- DERIV" expr))))

;;; 实现基本过程
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

(display-newline (deriv '(+ x 3) 'x))
(display-newline (deriv '(* x y) 'x))
(display-newline (deriv '(* (* x y) (+ x 3)) 'x))

;;; 集合 - 作为未排序的表

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (not (element-of-set? x set))
      (cons x set)
      set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define set1 '(1 3 2 a b d 4))
(define set2 '(4 3 5 d 6))
(display-newline (element-of-set? 'd set1))
(display-newline (element-of-set? 'd set2))
(display-newline (adjoin-set 'f set1))
(display-newline (intersection-set set1 set2))

;;; 集合 - 作为排序的表

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((> (car set) x) #f)
        ((= (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((= (car set1) (car set2)))
        (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2))
         (intersection-set set1 (cdr set2)))
        ((< (car set1) (car set2))
         (intersection-set (cdr set1) set2)))


(exit)

