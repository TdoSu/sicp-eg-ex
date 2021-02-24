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

(define t1
  (list (list 'a (expt 2 1)) (list 'b (expt 2 2)) (list 'c (expt 2 3))
        (list 'd (expt 2 4)) (list 'e (expt 2 5))))

(define t2
  (list (list 'a (expt 2 1)) (list 'b (expt 2 2)) (list 'c (expt 2 3))
        (list 'd (expt 2 4)) (list 'e (expt 2 5))
        (list 'f (expt 2 6)) (list 'g (expt 2 7)) (list 'h (expt 2 8))
        (list 'i (expt 2 9)) (list 'j (expt 2 10))))

(display-newline (expt 2 4))

(display-newline (generate-huffman-tree t1))
; (a b c d e)
;   (a b c d)
;     (a b c)
;       (a b)
;         a 00001 五位
;         b 0001
;       c 001
;     d 01
;   e 1

(display-newline (generate-huffman-tree t2))
; a 十位
; b
;   (a b)
;   c
;     (a b c)
;     d
;       (a b c d)
;       e
;         (a b c d e)
;         f
;           (a b c d e f)
;           g
;             (a b c d e f g)
;             h
;               (a b c d e f g h)
;               i
;                 (a b c d e f g h i)
;                 j
;                   (a b c d e f g h i j)

;;; 当字母表中频率按 2 的 n 次幂升高时,
;;; 最不频繁的符号需要 n 位 01 表示
;;; 最频繁的符号需要 1 位表示

(exit)

