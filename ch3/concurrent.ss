(load "utils.ss")

(display-newline "------ 并发 ----------")

; (parallel-execute <p1> <p2> ... <pi>)
; 每个 p 是一个没有参数的过程,
; 执行 parallel-execute 时, 每个 p 一个线程并发进行

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

;;; x 有五种可能
; 1. (set! m1 x)
; 2. (set! m2 x)
; 3. (set! x (* m1 m2))
; a. (set! a x)
; b. (set! x (+ a 1))
; 1ab23 --> 110
; 12ab3 --> 100
; 123ab --> 101
; ab123 --> 121
; a123b --> 11

; make-serializer 构造串行化组

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))

; s 修改过的所有 p 不会交错进行
; 结果只有 123ab ab123 两种

;;; 多个共享资源的串行化

(define (exchange account1 account2)
  ;;; 计算差值
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ;;; 从 1 取出
    ((account1 'withdraw) difference)
    ;;; 存入 2
    ((account2 'deposit) difference)))

;;; Peter 交换  a1 100, a2 40
;;; Paul 交换   a1 100, a3 30

; Peter set! difference (- a1 a2) --> 60
; Paul exchange a1 a3 --> a1->30 a3->100
; Peter 从 a1 取 60 就异常了

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ;;;  暴露 balance-serializer  
            ((eq? m 'serializer) balance-serializer)
            (else (error 'MAKE-ACCOUNT-AND-SERIALIZER
                         "Unknown request"
                         m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(exit)

