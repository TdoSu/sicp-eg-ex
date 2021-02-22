(load "utils.ss")

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (if (null? set)
      #f
      (let ((e (entry set)))
        (cond ((= e x) #t)
              ((> e x) (element-of-set? x (left-branch set)))
              ((< e x) (element-of-set? x (right-branch set)))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x '() '()))
        ((= x (entry set)) set)
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))))

;;; 比较两个把树展开为表的过程

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (copy-to-list tree result-list)
  (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))

(define (tree->list-2 tree)
  (copy-to-list tree '()))

(trace copy-to-list)

(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(trace tree->list-1)

(display-newline (tree->list-1 t1))
; (append (tree->list-1 '(3 (1 () ()) (5 () ())))
;         (cons 7 (tree->list-1 '(9 () (11 () ())))))
; (append (append (1 () ())
;                 (cons 3 (tree->list-1 '(5 () ()))))
;         (cons 7 (append ()
;                         (cons 9 (tree->list-1 (11 () ()))))))
; (append (append (append ()
;                         (cons 1 ()))
;                 (cons 3 (append ()
;                                 (cons 5 ()))))
;         (cons 7 (append ()
;                         (cons 9 (append ()
;                                         (cons 11 ()))))))
; (1 3 5 7 9 11)
; (display-newline (tree->list-1 t2))
; (1 3 5 7 9 11)
; (display-newline (tree->list-1 t3))
; (1 3 5 7 9 11)

(display-newline (tree->list-2 t1))
; (display-newline (tree->list-2 t2))
; (display-newline (tree->list-2 t3))
; (1 3 5 7 9 11)

;;; 当树平衡时, 第二种方法增长的慢一些

(exit)

