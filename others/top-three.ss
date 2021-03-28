;;; 输出十个人中最高分数的前三个人的分数 - 由高到低

;;; 方案1 -- 非常烂... 全无性能可言
(define (sort items)
  (cond ((null? items) '())
        ((null? (cdr items)) items)
        ((null? (cdr (cdr items)))
         (if (> (car items) (cadr items))
             items
             (list (cadr items) (car items))))
        (else (let ((current (car items))
                    (rest (sort (cdr items))))
                (if (> current (car rest))
                    (cons current rest)
                    (cons (car rest) (sort (cons current (cdr rest)))))))))

;;; 方案2 -- 好一点点

(define (insert-element x l)
  (cond ((null? l) (cons x l))
        ((> x (car l)) (cons x l))
        (else (cons (car l) (insert-element x (cdr l))))))

(trace insert-element)

(define (sort items)
  (if (null? items)
      '()
      (insert-element (car items) (sort (cdr items)))))

(trace sort)

(define (top-three items)
  (let ((sorted-items (sort items)))
    (list (car sorted-items) (cadr sorted-items) (caddr sorted-items))))

; (sort '(4 21 3))

;;; 动态规划方案 -- O(n)

(define (iter a b c l)
  (cond ((null? l) (list a b c))
        ((> (car l) a) (iter (car l) a b (cdr l)))
        ((> (car l) b) (iter a (car l) b (cdr l)))
        ((> (car l) c) (iter a b (car l) (cdr l)))
        (else (iter a b c (cdr l)))))

(define (top-three items)
  (iter 0 0 0 items))

(trace iter)

(display (top-three '(25 36 4 55 71 18 0 71 89 65)))
(newline)

(exit)

