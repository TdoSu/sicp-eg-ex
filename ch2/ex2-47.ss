(load "utils.ss")

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin f) (car f))

(define (edge1 f) (cadr f))

(define (edge2 f) (caddr f))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin f) (car f))

(define (edge1 f) (cadr f))

(define (edge2 f) (cdr (cdr f)))

(exit)

