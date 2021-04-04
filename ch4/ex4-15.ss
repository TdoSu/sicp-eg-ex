(load "utils.ss")

;;; 图灵停机问题

;;; 不能实现一个过程 halts?
;;; 对于任意表达式 (p a)
;;; (halts? (p a)) 总可以返回 true/false 表示 (p a) 总是可以返回一个值

;;; 假设有 halts?

(define (run-forever)
  (fun-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(try try)

;;; (try try) 能停机
;;; ==> 可以走到 'halted 这个分支,
;;; ==> (halts? try try) 就是 false
;;; ==> (try try) 不能停机, 和假设矛盾

;;; (try try) 不能停机
;;; ==> (halts? try try) 是 true
;;; ==> (try try) 能停机, 和假设矛盾

;;; 所以不存在 halts? 这样的过程.

(exit)

