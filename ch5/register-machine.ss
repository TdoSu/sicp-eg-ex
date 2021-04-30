(load "utils.ss")

(define gcd-machine
  (make-machine
    ;;; 寄存器名字列表
    '(a b t)
    ;;; 基本操作
    (list (list 'rem remainder) (list '= =))
    ;;; 控制器
    '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
       gcd-done)))

(set-register-contents! gcd-machine 'a 206)

(set-register-contents! gcd-machine 'b 40)

(start gcd-machine)

(get-register-contents gcd-machine 'a)

(exit)

