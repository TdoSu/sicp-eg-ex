(load "utils.ss")

;;; 计算无穷连分式

;;; 递归

(define (cont-frac n d k)
  ;;; 子问题是一个 counter ~ k 的连分式求和
  (define (iter counter)
    (if (= counter k)
        (/ (n k) (d k))
        (/ (n counter)
           (+ (d counter)
              (iter (+ counter 1))))))
  (iter 1))

;;; 迭代
(define (cont-frac n d k)
  (define (iter counter result)
    (if (= counter 1)
        result
        (iter (- counter 1)
              (/ (n (- counter 1))
                 (+ (d (- counter 1)) result)))))
  (iter k (/ (n k) (d k))))


(display-newline (cont-frac (lambda (i) 1.0)
                            (lambda (i) 1.0)
                            1000))
;;; 1/phi

;;; 获取四位精度的 1/phi

(define (1/phi-4)
  (define (close-enough? x y) (< (abs (- x y)) 0.0001))
  (define i->1.0 (lambda (i) 1.0))
  (define (iter times)
    (let ((current (cont-frac i->1.0 i->1.0 times))
          (next (cont-frac i->1.0 i->1.0 (+ times 1))))
      (if (close-enough? current next)
          (begin (display-newline times)
                 current)
          (iter (+ 1 times)))))
  (iter 1))

(display-newline (1/phi-4))
;;; 计算 10 次可以满足 4 位精度

(exit)

