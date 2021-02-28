(load "utils.ss")

;;; 三种构建多实现数据系统的方案

;;; 基于类型分派
;;; 让数据带有类型标识, 根据类型标识, 分派给不同的操作 
;;; contents add-tag

; (define real-part-rect ...)
; (define real-part-polar ...)

(define (real-part z)
  (cond ((eq? (tag z) 'rect) (real-part-rect (content z)))
        ((eq? (tag z) 'polar) (real-part-polar (content z)))
        (else (error 'REAL-PART "没有这个类型的操作 " z))))
;;; 要添加一种类型, 那么类型标识和类型名, 不能和其他类型重合, 需要修改每一个通用操作过程 
;;; 要添加一个操作, 每个类型都需要实现通用操作, 再实现通用操作 过程

;;; 数据导向
;;; 创建类型和操作的二维表格, 根据数据的类型标识和操作进行查表获取真实的操作过程

(define (install-rect-package)
  ; (define real-part ...)
  (put 'real-part '(rect) real-part)
  'done)
;;; 添加类型只需要实现对应的包, 不需要改动之前通用过程代码
;;; 添加一个操作就比较麻烦了, 每个包代码都要改一下, 不过不用担心重名问题

;;; 消息传递
;;; 创建一个类型对象, 对象中含有类型对应的各种操作

(define (make-complex-from-r-a r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) r)
          ;;;
          (else (error 'make-complex (list op r a)))))
  dispatch)
;;; 添加一个类型写对应的类型操作部分代码就行了, 需要修改通用过程, 但不用修改各个过程
;;; 也不用担心过程重名
;;; 添加过程也需要修改每个类型

;;; 感觉添加过程差不多, 添加类型 数据导向 消息传递 简单一些

(exit)

