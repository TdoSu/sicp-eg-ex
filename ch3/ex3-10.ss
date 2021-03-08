(load "utils.ss")

(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds")))
   initial-amount))

;;; 一个 lambda 表达式的值就是两个圈圈, 两个指针, 在哪个环境计算环境指针就指向哪
;;; 这里有三个 lambda 表达式
(define make-withdraw
  ;;; 全局计算的 lambda -- 指向 global
  (lambda (initial-amount)
    ;;; E1 计算的 lambda -- 指向 E1
    ((lambda (balance)
       ;;; E2 计算的 lambda -- 指向 E2
       (lambda (amount)
         (if (>= balance amount)
             (begin (set! balance (- balance amount))
                    balance)
             "Insufficient funds")))
     initial-amount)))

(define W1 (make-withdraw 100))

(W1 50)

(define W2 (make-withdraw 100))

(exit)

