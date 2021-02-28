(load "utils.ss")

;;; 用消息传递的风格重写 make-from-mag-ang

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)))
  dispatch)

;;; make-from-mag-ang 根据 r a 创建一个对象 dispatch
;;; dispatch 根据给他的不同消息, 以及天生携带的 r a 返回不同值

(exit)

