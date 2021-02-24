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

; (define sample-tree
;   (make-code-tree
;     (make-leaf 'A 4)
;     (make-code-tree
;       (make-leaf 'B 2)
;       (make-code-tree (make-leaf 'D 1)
;                       (make-leaf 'C 1)))))

; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (display-newline (decode sample-message sample-tree))

; ((A B C D) 8)
;   左 (A 4)
;   右 ((B C D) 4)
;     (B 2)
;     ((C D) 2)
;       (D 1)
;       (C 1)

; ADABBCA

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error 'ENCODE-SYMBOL "tree 不包含 symbol" symbol tree))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error 'CHOOSE-BRANCH "bad bit" bit))))

(display-newline
  (decode (encode
            '(A B D C B A C)
            ; (decode sample-message
            ;         (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1))))
            (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1))))
          (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))))

(define test-list '((A 3) (B 5) (D 8) (E 2) (C 1) (H 1) (F 12)))

(display-newline (generate-huffman-tree test-list))

;;; ----------------

(define ALPHABET
  '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define t (generate-huffman-tree ALPHABET))

(define m
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(display-newline (decode (encode m t) t))
(display-newline (length (encode m t)))
;;; 84 个二进制位

;;; 8 个符号, 定长编码 2^3 = 8, 三位一个符号
;;; 3 * 36 = 108 个二进制位

(exit)

