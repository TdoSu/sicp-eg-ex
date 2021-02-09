(load "utils.ss")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(display-newline (gcd 206 40))

;;; 应用序和正则序产生的计算过程, 以及计算了多少次 remainder

;;; 正则序
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; 计算 (remainder 206 40) -> 6不等于0
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; 计算 (remainder 40 (remainder 206 40)) -> 4不等于0
; (gcd (remainder 40 (remainder 206 40))
;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; 计算 (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) -> 2不等于0
; (gcd
;   (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;   (remainder
;     (remainder 40 (remainder 206 40))
;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; )
; 计算 (remainder
;   (remainder 40 (remainder 206 40))
;   (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) -> 0
; 计算 (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) -> 2
; remainder 计算了18 次

;;; 应用序
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (gcd 40 6)
; (gcd 6 (remainder 40 6))
; (gcd 6 4)
; (gcd 4 (remainder 6 4))
; (gcd 4 2)
; (gcd 2 (remainder 4 2))
; 2
; remainder 计算了 4 次

(exit)

