(load "utils.ss")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (define (insert-leaf new-leaf leafs)
    (cond ((null? leafs) (list new-leaf))
          ((<= (weight new-leaf) (weight (car leafs)))
           (cons new-leaf leafs))
          (else
            (cons (car leafs) (insert-leaf new-leaf (cdr leafs))))))
  ;;; 每次拿到前两个叶子归并, 然后放入之前的 leafs
  ;;; 当 leafs 只有一个元素时返回
  (if (null? (cdr leafs))
      (car leafs)
      (successive-merge
        (insert-leaf (make-code-tree (car leafs) (cadr leafs))
                     (cddr leafs)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (+ (weight (left-branch tree))
         (weight (right-branch tree)))))

(define test-list '((A 3) (B 5) (D 8) (E 2) (C 1) (H 1) (F 12)))

(display-newline (generate-huffman-tree test-list))

(exit)

