(load "utils.ss")

;;; 利用不动点求 phi

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x)))
                         1.0))

(display-newline phi)
;;; 1.6180327868852458

(exit)

