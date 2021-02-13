(load "utils.ss")

;;; 活动体平衡

; (define (make-mobile left right) (list left right))
; (define (left-branch m) (car m))
; (define (right-branch m) (cadr m))

; (define (make-branch length structure) (list length structure))
; (define (branch-length b) (car b))
; (define (branch-structure b) (cadr b))

(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else (+ (total-weight (branch-structure (left-branch m)))
                 (total-weight (branch-structure (right-branch m)))))))

(define (balance? m)
  (define (moment b)
    (* (branch-length b)
       (total-weight (branch-structure b))))
  (cond ((null? m) #t)
        ((not (pair? m)) #t)
        ((= (moment (left-branch m)) (moment (right-branch m)))
         (and (balance? (branch-structure (left-branch m)))
              (balance? (branch-structure (right-branch m)))))
        (else #f)))

;;; 修改活动体的实现, 对应修改选择函数和构造函数就可以了
(define (make-mobile left right) (cons left right))
(define (left-branch m) (car m))
(define (right-branch m) (cdr m))
(define (make-branch length structure) (cons length structure))
(define (branch-length b) (car b))
(define (branch-structure b) (cdr b))

(define m (make-mobile (make-branch 3 4)
                       (make-branch 2 (make-mobile (make-branch 1 2)
                                                   (make-branch 3 2)))))

(display-newline (total-weight m))

(display-newline (balance? m))

(define m2 (make-mobile (make-branch 3 6)
                       (make-branch 2 (make-mobile (make-branch 4 7)
                                                   (make-branch 14 2)))))

(display-newline (balance? m2))

(exit)

