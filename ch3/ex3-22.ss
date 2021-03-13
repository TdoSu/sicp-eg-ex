(load "utils.ss")

;;; 用带有局部状态的过程实现队列

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error 'FRONT-QUEUE "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (if (empty-queue?)
            (begin (set! front-ptr new-pair)
                   (set! rear-ptr new-pair)
                   dispatch)
            (begin (set-cdr! rear-ptr new-pair)
                   (set! rear-ptr new-pair)
                   dispatch))))
    (define (delete-queue!)
      (if (empty-queue?)
          (error 'DELETE-QUEUE! "DELETE! called with an empty queue")
          (begin (set! front-ptr (cdr front-ptr))
                 dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)))
    dispatch))

(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) ((q 'delete-queue!)))
(define (front-queue q) ((q 'front-queue)))

(define q (make-queue))
(insert-queue! q 'a)
(display-newline (front-queue q))
; a
(delete-queue! q)
(insert-queue! q 'b)
(display-newline (front-queue q))
; b
(insert-queue! q 'c)
(delete-queue! q)
(display-newline (front-queue q))
; c

(exit)

