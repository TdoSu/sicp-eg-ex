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

(define (make-wire)
  ;;; 线路有两个状态
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= new-value signal-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    ;;; 线路有两个定义的行为
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error 'WIRE "Unknown operation" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin (car procedures)
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((firt-item (firt-agenda-item the-agenda)))
        (firt-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (displayh name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value =  ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))

(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
; > sum 0 New-value = 0
(probe 'carry carry)
; > carry 0 New-value = 0

(half-adder input-1 input-2 sum carry)
; 'ok
(set-signal! input-1 1)
; 'done
(propagate)
; > sum 8 New-value = 1
; 'done

(set-signal! input-2 1)
; 'done

(propagate)
; > carry 11 New-value = 1
; > sum 16 New-value = 0
; 'done

(exit)

