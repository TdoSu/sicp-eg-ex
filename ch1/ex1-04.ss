(load "utils.ss")

; 描述下面过程的行为
((define (a-plus-abs-b a b))
 ((if (> b 0) + -) a b))

; a 加上 b 的绝对值:
; 如果 b 大于 0, 计算 a + b
; 否则, 计算 a - b

(exit)

