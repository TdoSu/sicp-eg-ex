(load "utils.ss")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          ;;; 如果有类型列表对应的操作, 直接应用这个操作
          (apply proc (map contents arts))
          ;;; 没有对应操作, 就尝试类型转换
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a1)))
                        (else
                          (error 'APPLY-GENERIC "No methods for these types" (list op type-args))))))
              (error 'APPLY-GENERIC "No methods for these types" (list op type-tags)))))))

;;; 如果定义了同类型的转换, 该类型又没有对应操作
;;; 首先因为没有对应操作, 会尝试进行类型转换,
;;; 因为有类型转换的方式 (同类型转换), 所以完成类型转换之后, 会递归再次调用 apply-generic
;;; 整个过程就陷入了死循环.

;;; 产生这问题的主要原因是, 第一步就已经查找了所有可能存在的操作,
;;; 同类型转换没有改变查找依赖的类型, 所以再查找一次也还是找不到对应的操作.

;;; 如果要引入同类型转换, 那么可以在进行类型转换前,
;;; 先进行判断, 如果没有对应操作直接报错.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          ;;; 如果有类型列表对应的操作, 直接应用这个操作
          (apply proc (map contents arts))
          ;;; 没有对应操作, 就尝试类型转换
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond ((eq? type1 type2)
                         (error 'APPLY-GENERIC "没有对于该类型的操作") (list type1 op))
                        (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a1)))
                        (else
                          (error 'APPLY-GENERIC "No methods for these types" (list op type-args))))))
              (error 'APPLY-GENERIC "No methods for these types" (list op type-tags)))))))

(exit)

