(load "utils.ss")

;;; 7 次密码错误, 报警

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define dispatch
    (let ((count 0))
      (define (reset-count) (set! count 0))
      (lambda (ps m)
        ;;; 在这里校验一下密码
        (cond ((not (eq? ps password))
               (lambda (x)
                 (cond ((>= count 7) "报警啦....")
                       (else (begin (set! count (+ count 1))
                                    "Incorrect password")))))
              ((eq? m 'withdraw) (begin (reset-count) withdraw))
              ((eq? m 'deposit) (begin (reset-count) deposit))
              (else (error 'MAKE-ACCOUNT
                           "Unknown request"
                           m))))))
  dispatch)

(define acc (make-account 100 'secret-password))

(display-newline ((acc 'secret-password 'withdraw) 40))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'secret-password 'withdraw) 40))
(display-newline ((acc 'some-other-password 'deposit) 50))
(display-newline ((acc 'some-other-password 'deposit) 50))

(exit)

