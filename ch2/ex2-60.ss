(load "utils.ss")

;;; 集合作为有重复的表

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2) (append set1 set2))

(define set1 '(1 3 2 a b d 4))
(define set2 '(4 3 5 d 6))

(display-newline (union-set set1 set2))

; 允许重复时
; adjoin-set 的时间复杂度非常小 O(1)
; union-set 时间复杂度 O(n), n 是 其中一个集合的元素个数 (因为用到了 append)
; intersection-set 的时间复杂度 O(n^2) -- element-of-set? 更麻烦了, 因为元素可能重复
;;; 在需要频繁插入元素合并集合的场合这个更好用一些, 但需要频繁判断元素是否属于集合就不方便了

(exit)

