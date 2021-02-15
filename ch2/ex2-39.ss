(load "utils.ss")

;;; 分别利用 fold-right fold-left 实现 reverse

(define (accumulate op initial items)
  (if (null? items)
      initial
      (op (car items)
          (accumulate op initial (cdr items)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        ;;; 注意先 r 后 c
        ;;; js 的 reduce 就是左折叠
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (c r)
                (append r (list c)))
              '()
              sequence))

(define (reverse sequence)
  (fold-left (lambda (r c) (cons c r))
             '()
             sequence))

(define s (list 1 2 3 4))

(display-newline (reverse s))

(exit)

