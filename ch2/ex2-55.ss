(load "utils.ss")

;;; 解释下面表达式的求值结果
(display-newline (car ''abracadabra))

; (car ''abracadabra)
; (car (quote 'abracadabra))
; (car (quote (quote abracadabra)))
; (car '(quote abracadabra))
; quote

;;; 至此我们知道有如下过程可以对 quote 返回的值进行操作
;;; car cdr eval define eq? equal?

(display-newline (+ 1 '2))
;;; 3
;;; (quote <number>) 会直接得到 <number> 而不是 symbol, 所以可以用于 + - * / remainder =

(define b 32)

(display-newline (and 'a 'b))
;;; b
;;; 结果是 b, 而不是 32, b 不会被求值

(display-newline (+ 1 'a))
;;; a is not a number

(exit)

