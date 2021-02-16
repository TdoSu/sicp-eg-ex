(load "utils.ss")

;;; 八皇后问题一个错误解法的分析
;;; x 行 y 行位置反了

(define (queens board-size)
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

                   ;;; flatmap 中 lambda 这个过程会执行 board-size 次
                   ;;; queen-cols 这个递归过程放在这里
                   ;;; 这会导致整个程序变成之前顺序的 n! 倍

                   ;;; k-1 列的所有解
                   (queen-cols (- k 1)))) ; x
            ;;; 一共 board-size 行
            (enumerate-interval 1 board-size))))) ; y
  (queen-cols board-size))

(exit)

