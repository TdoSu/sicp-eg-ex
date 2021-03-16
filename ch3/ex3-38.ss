(load "utils.ss")

; 123, 132, 213, 231, 312, 321
; 45, 35, 45, 50, 40, 40
; 四种可能结果

(let ((balance 100))
  (begin
    (set! balance (+ balance 10))
    (set! balance (- balance 20))
    (set! balance (- balance (/ balance 2)))
    (display-newline balance)
  )
)
; 45

(let ((balance 100))
  (begin
    (set! balance (+ balance 10))
    (set! balance (- balance (/ balance 2)))
    (set! balance (- balance 20))
    (display-newline balance)
  )
)
; 35

(let ((balance 100))
  (begin
    (set! balance (- balance 20))
    (set! balance (+ balance 10))
    (set! balance (- balance (/ balance 2)))
    (display-newline balance)
  )
)
; 45

(let ((balance 100))
  (begin
    (set! balance (- balance 20))
    (set! balance (- balance (/ balance 2)))
    (set! balance (+ balance 10))
    (display-newline balance)
  )
)
; 50

(let ((balance 100))
  (begin
    (set! balance (- balance (/ balance 2)))
    (set! balance (+ balance 10))
    (set! balance (- balance 20))
    (display-newline balance)
  )
)
; 40

(let ((balance 100))
  (begin
    (set! balance (- balance (/ balance 2)))
    (set! balance (- balance 20))
    (set! balance (+ balance 10))
    (display-newline balance)
  )
)
; 40

(display-newline "--------------------------")

(let ((balance 100)
      (a1 0)
      (a2 0)
      (a3 0)
      (a4 0))
  (begin
    (set! a1 (+ balance 10))
    (set! a3 (/ balance 2))
    (set! a4 (- balance a3))
    (set! a2 (- balance 20))
    (set! balance a3)
    (set! balance a2)
    (set! balance a1)
    ;;; 123 --> 50
    ;;; 132 --> 80
    ;;; 213 --> 50
    ;;; 231 --> 110
    ;;; 312 --> 80
    ;;; 321 --> 110
    (display-newline balance)
  )
)

(exit)

