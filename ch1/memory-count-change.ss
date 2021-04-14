(define (display-newline . args)
  (if (null? args)
      (newline)
      (begin (display (car args))
             (display " ")
             (apply display-newline (cdr args)))))

;;; 如何利用剪枝尽可能优化硬币找零
;;; 1. 如果硬币比钱数大, 直接跳过
;;; 2. 如果已经计算过, 缓存, 下次不在计算

(define moneys (list 50 25 10 5 1))

(define (lookup key1 key2 items)
  (cond ((null? items) #f)
        ((and (= key1 (caar items)) (= key2 (cadar items))) (caddar items))
        (else (lookup key1 key2 (cdr items)))))

(define memory '())

(define (iter count coins)
  (cond ((null? coins) 0)
        ;;; 唉, 这句顺序很重要, 如果写在下一句下面, 就不好使了
        ((= count 0)
         (begin (set! memory (cons (list count (car coins) 1) memory))
                1))
        ((= (car coins) 1) 1)
        ;;; 依赖 coins 由大到小排序
        ((> (car coins) count) (iter count (cdr coins)))
        (else (let ((target (lookup count (car coins) memory)))
                (if target
                    target
                    (let ((result (+ (iter (- count (car coins)) coins)
                                     (iter count (cdr coins)))))
                      (begin (set! memory (cons (list count (car coins) result) memory))
                             result)))))))

(define (cc total moneys)
    (iter total moneys))

;;; 硬币找零
(define (count-change total)
  (cc total moneys))

(trace iter)

(display-newline "100 cc:" (count-change 100))

(exit)

