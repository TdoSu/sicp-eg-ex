(load "utils.ss")

;;; 查询系统的实现

;;; 驱动循环

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           ;;; 添加断言
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
            (newline)
            (display output-prompt)
            ;;; 求值 -- 推理
            (display-stream
              (stream-map
                (lambda (frame)
                  (instantiate q
                               frame
                               (lambda (v f)
                                 (contract-question-mark v))))
                (qeval q (singleton-stream '()))))
            (query-driver-loop)))))

(define (instantiate expr frame unbound-var-handler)
  (define (copy expr)
    (cond ((var? expr)
           (let ((binding (binding-in-frame expr frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler expr frame))))
          ((pair? expr)
           (cons (copy (car expr)) (copy (cdr expr))))
          (else expr)))
  (copy expr))

;;; 求值器


(exit)

