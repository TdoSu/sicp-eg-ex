(load "utils.ss")

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(display-newline (tree-map square '(1 (2 (3 4) 5))))

(exit)

