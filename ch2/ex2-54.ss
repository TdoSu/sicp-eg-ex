(load "utils.ss")

;;; 实现比较两个表的 equal? 过程

(define (equal? l1 l2)
  (cond ((and (not (pair? l1)) (not (pair? l2)))
         (cond ((and (number? l1) (number? l2)) (= l1 l2))
               ((or (and (symbol? l1) (number? l2))
                    (and (number? l1) (symbol? l2)))
                #f)
               ((and (not (number? l1)) (not (number? l2))) (eq? l1 l2))
               (else #f)))
        ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else #f)))

(display-newline (equal? 1 2))
;;; > #f
(display-newline (equal? 1 1.0))
;;; > #t
(display-newline (equal? '(a b 2 1 c) '(a (b 2) 1 c)))
;;; > #f
(display-newline (equal? '(a b 2 1 c) '(a b 2 1 c)))
;;; > #t

(define a 'a)
(define b 'b)
(display-newline (equal? '(a b 2 1 c) (list a b 2 1 'c)))
;;; > #t

(display-newline "-------------")

;;; 下面这个真没办法了, 原生的 1 '1 也是相同的
(display-newline (equal? 1 '1))
;;; > #t
(display-newline (symbol? '1))
;;; > #f

(exit)

