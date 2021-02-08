(load "utils.ss")

;;; sine 的一种求值方法和时间空间复杂度

;;; x 足够小时, sine 约等于 x
;;; sine(x) = 3 * sine(x/3) - 4 * sine^3(x/3) 可以缩小 x

(define (cube x) (* x x x))

(define (sine angle)
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(trace sine)

(display-newline (sine 12.15))

; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.35)))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine 0.05))))))

;;; p 计算了 5 次

;;; sine 时间复杂度 O(log3(a)) 空间复杂度 O(log3(a))

(exit)

