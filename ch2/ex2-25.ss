(load "utils.ss")

;;; 取出 7

(display-newline (car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))))
; (1 3 (5 7) 9)
; (3 (5 7) 9)
; ((5 7) 9)
; (5 7)
; (7)
; 7

(display-newline (car (car '((7)))))
; ((7))
; (7)
; 7

(display-newline
  (cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7)))))))))))))
; (1 (2 (3 (4 (5 (6 7))))))
; (2 (3 (4 (5 (6 7))))) cadr
; (3 (4 (5 (6 7)))) cadr
; (4 (5 (6 7))) cadr
; (5 (6 7)) cadr
; (6 7) cadr
; 7 cadr

(exit)

