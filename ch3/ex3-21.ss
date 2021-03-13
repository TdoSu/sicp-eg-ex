(load "utils.ss")

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
(delete-queue! q)
(delete-queue! q)
;;; 这时候, 队列的尾指针仍然指向 'c
;;; 当队列再次插入值的时候才会被重置
(display-newline q)
;;; > (() c)

;;; 给队列实现一个打印方法

(define (print-queue queue)
  (define (iter items)
    (if (not (null? items))
        (begin (display (car items))
               (if (not (null? (cdr items))) (display " "))
               (iter (cdr items)))))
  (display "Queue: (")
  (iter (front-ptr queue))
  (display ")")
  (newline))

(print-queue q)
(insert-queue! q 'a)
(insert-queue! q 'b)
(insert-queue! q 'c)
(print-queue q)

(exit)

