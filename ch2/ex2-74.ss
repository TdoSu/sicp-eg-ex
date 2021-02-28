(load "utils.ss")


;;; 不同分支对应不同 file
;;; 不同 file 根据 employee-name 采用不同的查询方式可以查到不同人

(get-record 'weifubao)

(define (get-record employee)
  ;;; 根据 部门标识 获取对应的查找记录过程, 然后用于雇员名称
  ((get 'record (apartment employee))
   (name employee)))

(display-newline "Hello, scheme!")

(exit)

