(load "utils.ss")

;;; 将区间乘法拆解为 9 中情况, 减少内部乘法计算次数

;;; x1 x2 y1 y2   min     max
;;; -----------------------------------------------------
;;; +  +  +  +  (* x1 y1) (* x2 y2)
;;; -  -  +  +  (* x1 y2) (* x2 y1)
;;; -  +  +  +  (* x1 y2) (* x2 y2)
;;; -  -  -  +  (* x1 y2) (* x1 y1)
;;; +  +  -  -  (* x2 y1) (* x1 y2)
;;; +  +  -  +  (* x2 y1) (* x2 y2)
;;; -  +  -  -  (* x2 y1) (* x1 y1)
;;; -  -  -  -  (* x2 y2) (* x1 y1)

;;; -  +  -  +  (min (* x1 y2) (* x2 y1)) (max (* x2 y2) (* x1 y1))
;;; 减少乘法的次数改变 min 和 * 的顺序
;;; -  +  -  +  (* (min x1 y1) (max x2 y2)) (max (* x2 y2) (* x1 y1))

(define (pos x)
	(> x 0)
)

(define (mul-interval x y)
	(cond ((and (pos (upper-bound x))
              (pos (lower-bound x))
              (pos (upper-bound y))
              (pos (lower-bound y)))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
				((and (pos (upper-bound x))
              (pos (lower-bound x))
              (pos (upper-bound y))
              (not (pos (lower-bound y))))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
				((and (pos (upper-bound x))
              (pos (lower-bound x))
              (not (pos (upper-bound y)))
              (not (pos (lower-bound y))))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (power-bound y))))
				((and (pos (upper-bound x))
              (not (pos (lower-bound x)))
              (pos (upper-bound y))
              (pos (lower-bound y)))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (upper-bound y))))
				((and (pos (upper-bound x))
              (not (pos (lower-bound x)))
              (pos (upper-bound y))
              (not (pos (lower-bound y))))
         (let ((p1 (* (lower-bound x) (lower-bound y)))
               (p2 (* (lower-bound x) (upper-bound y)))
               (p3 (* (upper-bound x) (lower-bound y)))
               (p4 (* (upper-bound x) (upper-bound y))))
           (make-interval (min p1 p2 p3 p4)
                          (max p1 p2 p3 p4))))
				((and (pos (upper-bound x))
              (not (pos (lower-bound x)))
              (not (pos (upper-bound y)))
              (not (pos (lower-bound y))))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (lower-bound y))))
				((and (not (pos (upper-bound x)))
              (not (pos (lower-bound x)))
              (pos (upper-bound y))
              (pos (lower-bound y)))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (lower-bound y))))
				((and (not (pos (upper-bound x)))
              (not (pos (lower-bound x)))
              (pos (upper-bound y))
              (not (pos (lower-bound y))))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
				((and (not (pos (upper-bound x)))
              (not (pos (lower-bound x)))
              (not (pos (upper-bound y)))
              (not (pos (lower-bound y))))
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))))

;;; 因为区间顺序, 以下情况不存在
;;; -  -  +  -  ----
;;; +  -  +  +  ----
;;; +  +  +  -  ----
;;; +  -  -  -  ----
;;; +  -  -  +  ----
;;; +  -  +  -  ----
;;; -  +  +  -  ----

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (print-interval i)
  (begin (display "[")
         (display (lower-bound i))
         (display ", ")
         (display (upper-bound i))
         (display "]")))

(define i1 (make-interval 1 2))
(define i2 (make-interval 0 3))

(print-interval (mul-interval i1 i2))
(newline)

(exit)
