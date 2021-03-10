(load "utils.ss")

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

; x --> [a, -]-->[b, -]-->[c, /]
; (last-pair x) --> [c, /]
; (set-cdr! (last-pair x) x)
; [a, -]-->[b, -]-->[c, -]--> 
;  ^                        |
;  | ------------------------

; 所以计算 (last-pair z) 会陷入死循环, 因为不存在满足 (null? (cdr x)) 的那个部分

(display-newline (last-pair z))

(exit)

