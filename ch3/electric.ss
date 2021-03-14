(load "utils.ss")

(display-newline "-------- 电路模拟 --------")

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define f (make-wire))

;;; 有一种说法, 说变量就像电线, 反过来, 这里的电线就像是变量

;;; 半加器

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;;; 全加器

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c-2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error 'LOGICAL-NOT "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok) 

(define (logical-and v1 v2)
  (cond ((or (= v1 1) (= v2 1)) 1)
        ((and (= v1 0) (= v2 0)) 0)
        (else 'LOGICAL-AND "Invalid signal" (list v1 v2))))

(define (or-gate a1 a2 output)
  (let ((s1 (make-wire))
        (s2 (make-wire)))
    (inverter a1 s1)
    (inverter a2 s2)
    (and-gate s1 s2 output)
    'ok))

(exit)

