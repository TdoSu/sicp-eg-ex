(load "utils.ss")

;;; 求所有三元数组 (i j k), i j k 小于 s 不相等并且和是 s

(define (three-numbers-list s)
  ;;; 过滤和是 s 的三元组
  (filter (lambda (numbers) (= (accumulate + 0 numbers) s))
          ;;; 获取所有三元组
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 (- s 1)))))

(display-newline (three-numbers-list 10))

(exit)

