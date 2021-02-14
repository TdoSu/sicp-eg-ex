(load "utils.ss")

;;; 利用 Horner 规则求多项式的值
;;; anx^n + a(n-1)x^(n-1) + ... + a1x + a0
;;; 转化为
;;; (... (anx + a(n-1))x + ... + a1)x + a0
;;; 也就是
;;; 从 an 开始, 乘以 x, 加上 an-1, 再乘以 x, 如此继续, 至到 a0.
;;; anx
;;; anx + an-1
;;; (anx + an-1)x
;;; (anx + an-1)x + a(n-2)
;;; ...

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(display-newline (horner-eval 2 (list 1 3 0 5 0 1)))
(display-newline (+ (* 1 (expt 2 0))
                    (* 3 (expt 2 1))
                    (* 0 (expt 2 2))
                    (* 5 (expt 2 3))
                    (* 0 (expt 2 4))
                    (* 1 (expt 2 5))))

(exit)

