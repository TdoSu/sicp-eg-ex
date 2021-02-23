(load "utils.ss")

;;; 利用二叉平衡树实现集合 并集 和 交集

;;; 1. 把两个二叉树集合展开成有序表
;;; 2. 然后合并有序表
;;; 3. 然后把有序表转化为新的二叉树集合

(define (union-set set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (display-newline list1 list2)
    (let ((result-list (union-set-by-list list1 list2)))
      (list->tree result-list))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (let ((result-list (intersection-set-by-list list1 list2)))
      (list->tree result-list))))

;;; ----------------

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

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      ;;; 长度 - 1 之后除以 2 取整数
      (let ((left-size (quotient (- n 1) 2)))
        ;;; 左侧结果
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list (right-branch tree))))))

(define (union-set-by-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set-by-list (cdr set1) (cdr set2))))
                      ((> x1 x2) (cons x2 (union-set-by-list set1 (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set-by-list (cdr set1) set2))))))))

(define (intersection-set-by-list set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((= (car set1) (car set2))
         (cons (car set1) (intersection-set-by-list (cdr set1) (cdr set2))))
        ((> (car set1) (car set2))
         (intersection-set-by-list set1 (cdr set2)))
        ((< (car set1) (car set2))
         (intersection-set-by-list (cdr set1) set2))))

(define s1 (list->tree '(1 3 4 5 7)))
(define s2 (list->tree '(1 2 5 7 8 9 10)))

(display-newline s1)
(display-newline s2)
(display-newline (union-set s1 s2))
(display-newline (intersection-set s1 s2))

(exit)

