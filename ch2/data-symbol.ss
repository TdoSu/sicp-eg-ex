(load "utils.ss")

;;; 符号数据

;;; 过程是一种延迟求值 (计算) 的手段
;;; 引号也是一种延迟求值的手段

;;; 加了引号之后 (+ 1 2) 并不会被求值
(define three '(+ 1 2))

;;; 至到作为 eval 参数才会被求值
(display-newline (eval three))

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(display-newline (memq 'apple '(pear banana prune)))
(display-newline (memq 'apple '(x (apple sauce) y apple pear)))

(exit)

