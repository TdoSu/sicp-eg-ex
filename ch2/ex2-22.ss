(load "utils.ss")

;;; 分析一个错误的迭代写法

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ;;; 这里先取出的元素会被合并到后面
              ; (cons (square (car things))
              ;       answer))))
              ;;; 这里由于 answer 第一元素是一个 '() 所以也有问题
              ; (cons answer
              ;       (square (car things))))))
              (append answer (list (square (car things)))))))
  (iter items '()))

(display-newline (square-list (list 1 2 3 4)))

(exit)

