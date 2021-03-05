(load "utils.ss")

;;; 带有密码的 account

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch ps m)
    ;;; 在这里校验一下密码
    (cond ((not (eq? ps password)) (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error 'MAKE-ACCOUNT
                       "Unknown request"
                       m))))
  dispatch)

(define acc (make-account 100 'secret-password))

(display-newline ((acc 'secret-password 'withdraw) 40))
(display-newline ((acc 'some-other-password 'deposit) 50))

(exit)

