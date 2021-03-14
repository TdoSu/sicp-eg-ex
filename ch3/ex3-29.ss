(load "utils.ss")

;;; 用与门和反门构造或门

;;; 德摩根定律
;;; (or v1 v2) --> (and (not v1) (not v2))

(define (or-gate a1 a2 output)
  (let ((s1 (make-wire))
        (s2 (make-wire)))
    (inverter a1 s1)
    (inverter a2 s2)
    (and-gate s1 s2 output)
    'ok))

(exit)

