(load "utils.ss")

;;; 八皇后问题

(define (make-position x y) (list x y))
(define (x-position p) (car p))
(define (y-position p) (cadr p))

(define (same-position p1 p2)
  (and (= (x-position p1) (x-position p2))
       (= (y-position p1) (y-position p2))))

(define (queens board-size)
  (define empty-board '())
  (define (adjoin-position r k items)
    (cons (make-position r k) items))
  (define (find-position-k k positions)
    (cond ((null? positions) '())
          ((= k (y-position (car positions))) (car positions))
          (else (find-position-k k (cdr positions)))))
  (define (some items predicate)
    (cond ((null? items) #f)
          ((predicate (car items)) #t)
          (else (some (cdr items) predicate))))
  (define (safe? k positions)
    (let ((position-k (find-position-k k positions)))
      (and (not (some positions
                      ;;; 相同行, 却不是同一个位置
                      (lambda (p)
                        (and (= (x-position p) (x-position position-k))
                             (not (same-position p position-k))))))
           (not (some positions
                      ;;; 行列差相等 (对角线) , 却不是同一个位置
                      (lambda (p)
                        (and (= (abs (- (x-position p) (x-position position-k)))
                                (abs (- (y-position p) (y-position position-k))))
                             (not (same-position p position-k)))))))))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            ;;; rest-of-queens 前 k-1 列放置 k-1 个皇后的一种方式
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     ;;; 添加新位置 new-row 行 k 列
                     (adjoin-position new-row k rest-of-queens))
                   ;;; 一共 board-size 行
                   (enumerate-interval 1 board-size)))
            ;;; k-1 列的所有解
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(display-newline (queens 8))
(display-newline (length (queens 8)))
; 92

;;; 这个问题的思路和 expmod 很像, 一边生成列表, 一边过滤, 避免了问题规模太大
;;; 所以加上 safe 之后, 反而比 safe 总为 #t 要快很多.

(exit)

