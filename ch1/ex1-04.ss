(load "utils.ss")

; 描述下面过程的行为

(define (a-plus-abs-b a b)
 ((if (> b 0) + -)
  a
  b))

; a-plus-abs-b 有两个参数 a b
; 过程体是一个组合式

; (
;   (if (> b 0) + -)
;   a
;   b
; )

; 根据组合式求值规则
; 首先对 a, b, (if (> b 0) + -) 求值
; (if (> b 0) + -) 又是一个组合式
; b > 0 时, 返回 +
; 否则, 返回 -
; 因此
; b > 0 --> (+ a b)
; b <= 0 --> (- a b)

(exit)

