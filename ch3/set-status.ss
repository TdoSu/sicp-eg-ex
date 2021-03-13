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

;;; 引进赋值带来的好处

;;; chez scheme 里面有一个过程 (random n) 会返回大于等于 0 小于 n 的数

;;; 线性同余随机发生器
(define (rand-update x)
  (let ((m (expt 2 32))
        (a 1664525)
        (b 1013904223))
    (remainder (+ (* a x) b) m)))

(define random-init 12)

; (define rand
;   (let ((x random-init))
;     (lambda ()
;       (set! x (rand-update x))
;       (remainder x 10000))))

(define (rand) (random 100000))

(display-newline "--- random ----")

(let loop ((i 0))
  (if (> i 10)
      'done
      (begin (display-newline (rand))
             (loop (+ i 1)))))

;;; 蒙特卡洛方法
;;; 利用大量随机数据反向估算某个值.

;;; 6/pi^2 是随机选取两个整数没有公共因子的概率, 反过来就可以用这个概率估算 pi.

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(display-newline (estimate-pi 1000000))
; 3.1412338222154714

(display-newline "---------- Queue ------------------")

;;; 队列 Queue
;;; 一端插入, 另一端删除
; (define q (make-queue))
; (insert-queue! q 'a)  ; a
; (insert-queue! q 'b)  ; a b
; (delete-queue! q)     ; b
; (insert-queue! q 'c)  ; b c
; (insert-queue! q 'd)  ; b c d
; (delete-queue! q)     ; c d

;;; 队列可以表示为一般的序列, 但这样添加队列就要遍历整个表找到表尾
;;; 另一个办法是用一个序对, car, cdr 分别指向队列头尾, 头尾操作就都只有 O(1) 的时间复杂度了

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(display-newline (empty-queue? (make-queue)))
; #t

(define (front-queue queue)
  (if (empty-queue? queue)
      (error 'FRONT-QUEUE "FRONT called with an empty queue")
      (car (front-ptr queue))))

; (display-newline (front-queue (make-queue)))
; 报错

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
        (begin (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)
        (begin (set-cdr! (rear-ptr queue) new-pair)
               (set-rear-ptr! queue new-pair)
               queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error 'DELETE-QUEUE! "DELETE! called with an empty queue")
      (begin (set-front-ptr! queue (cdr (front-ptr queue)))
             queue)))

(define q (make-queue))
(insert-queue! q 'a)
(display-newline q)
(insert-queue! q 'b)
(insert-queue! q 'c)
(display-newline q)
(delete-queue! q)
(display-newline q)

;;; 栈
;;; 同一端插入删除


(exit)

