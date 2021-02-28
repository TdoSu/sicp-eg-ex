(load "utils.ss")

;;; 实现适用于不同类型数据的 + - * /    ---   数据导向
;;; 整数 有理数 复数 代数表达式 ...

(define (add x y) (apply-generic 'add  x y))
(define (sub x y) (apply-generic 'sub  x y))
(define (mul x y) (apply-generic 'mul  x y))
(define (div x y) (apply-generic 'div  x y))

;;; 常规数

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

;;; 引入类型转换来处理不同类型的数据对象进行运算的问题

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;;; put-coercion 放入一个专门放类型转换的表格中

;;; 在通用操作中引入类型转换

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents arts))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a1)))
                        (else
                          (error 'APPLY-GENERIC "No methods for these types" (list op type-args))))))
              (error 'APPLY-GENERIC "No methods for these types" (list op type-tags)))))))

(define (scheme-number->radio n) (make-radio (contents r) 1))
(define (radio->complex r) (make-radio (contents r) 0))

(exit)

