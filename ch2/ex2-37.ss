(load "utils.ss")

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;; 矩阵运算

;;; 向量表示为数字的序列 (1 2 3 4)
;;; 矩阵表示未向量的序列 ((1 2 3 4) (4 5 6 6) (6 7 8 9))

;;; (dot-product v w) 返回 vi wi 乘积的和 (向量点乘)
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; (display-newline (dot-product '(1 2 3) '( 0 2 1)))

;;; (matrix-*-vector m v) 返回向量 t, 其中 ti = mij, vj 乘积的和
(define (matrix-*-vector m v)
  (map (lambda (line)
         (dot-product line v))
       m))

(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define v '(1 0 2 1))

(display-newline (matrix-*-vector m v))

;;; (transpose m) 返回矩阵 n, 其中 nij = mji
(define (transpose m) (accumulate-n cons '() m))

(display-newline (transpose m))

;;; (matrix-*-matrix m n) 返回矩阵 p, 其中 pij = mik nkj 乘积的和
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (line) (matrix-*-vector n line)) m)))

(define n '((1 3 3 1) (2 0 1 1) (4 2 2 0) (1 0 0 1)))

(display-newline (matrix-*-matrix m n))

(exit)

