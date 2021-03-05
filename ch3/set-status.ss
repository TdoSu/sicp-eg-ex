(load "utils.ss")

(display-newline "*** 赋值和局部状态 ***")

;;; 一个有状态的过程

(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(display-newline (withdraw 25))
(display-newline (withdraw 25))
(display-newline (withdraw 60))
(display-newline (withdraw 15))

;;; 一个过程产生两个状态不相关的过程

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

(display-newline (w1 50))
(display-newline (w2 70))
(display-newline (w2 40))
(display-newline (w1 40))

;;; 消息传递风格

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error 'MAKE-ACCOUNT
                       "Unknown request"
                       m))))
  dispatch)

;;; acc 就是一个对象
;;; 'withdraw 'deposit 是传递给它的消息
;;; 根据消息, 返回一个过程

(define acc (make-account 100))
(display-newline ((acc 'withdraw) 50))
(display-newline ((acc 'withdraw) 60))
(display-newline ((acc 'deposit) 40))
(display-newline ((acc 'withdraw) 60))

(exit)

