(load "utils.ss")

;;; 用 accumulate 重新定义 count-leaves

(define (count-leaves tree)
  (accumulate +
              0
              ;;; 把树映射为它的叶子数
              (map (lambda (node)
                     (if (not (pair? node))
                         1
                         (count-leaves node)))
                   tree)))

(display-newline (count-leaves '((1 2) (3 (4 5) 6) 7)))

(exit)

