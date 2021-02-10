(load "utils.ss")

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;;; 这里用 * 代替 square 会导致重复计算 expmod
         ;;; 本来每次 exp 缩小一倍, 现在多算了一遍, 又增加了一倍 -- 抵消了
         ;;; O(log(n)) 变成了 O(n)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(exit)

