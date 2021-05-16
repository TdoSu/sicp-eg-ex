(load "utils.ss")

(display-newline "---------- 实现表格 --------------")

; 带表头的表

; (define table (list '*table* (cons 'a 1) (cons 'b 2) (cons 'c 3)))
; (lookup 'a table) --> 1
; (lookup 'd table) --> #t

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))))
    'ok))

(define (make-table)
  (list '*table*))

(define table (make-table))
(insert! 'a 1 table)
(insert! 'b 2 table)
(insert! 'c 3 table)
(display-newline (lookup 'c table))
(display-newline (lookup 'a table))
(insert! 'c 4 table)
(display-newline (lookup 'c table))

;;; 二维表格

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 table)))
    (if subtable
        (let ((record (assoc key-2 subtable)))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table))))
    'ok))

;;; 消息传递方式定义

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 local-table)))
        (if subtable
            (let ((record (assoc key-2 subtable)))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table))))
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error 'TABLE "Unknown operation" m))))
    dispatch))


;;; TODO: 考虑搞一个 n 维表格 make-table
;;; 分别用类型分派, 数据导向, 消息传递写一下

(exit)

