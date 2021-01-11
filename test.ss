(define (display-newline . msgs)
  (if (null? msgs)
      (newline)
      (begin (display (car msgs))
             (display " ")
             (apply display-newline (cdr msgs)))))

;;; 产生一个数字列表 (递增)
(define (interval start end step)
  (if (> start end)
      '()
      (cons start (interval (+ start step) end step))))

;;; 找出符合条件的元素的 index
(define (find-index-of-list items predicate?)
  (cond ((null? items) -1)
        ((predicate? (car items)) 0)
        (else (+ 1 (find-index-of-list (cdr items) predicate?)))))

;;; 返回列表长度
(define (list-length items)
  (if (null? items)
      0
      (+ 1 (list-length (cdr items)))))

;;; 返回列表第 n 个元素
(define (ref-list items n)
  (cond ((null? items) '())
        ((= n 0) (car items))
        (else (ref-list (cdr items) (- n 1)))))

;;; 映射列表
(define (map-list proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map-list proc (cdr items)))))

;;; 过滤列表
(define (filter-list predicate? items)
  (cond ((null? items) '())
        ((predicate? (car items))
         (cons (car items) (filter-list predicate? (cdr items))))
        (else (filter-list predicate? (cdr items)))))

;;; 折叠列表
(define (accumulate-list op init items)
  (if (null? items)
      init
      (op (car items) (accumulate-list op init (cdr items)))))

;;; 拼接两个列表得到新的列表
(define (append-list l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append-list (cdr l1) l2))))

;;; -------------------------- TODO --------------------------------

(exit)

