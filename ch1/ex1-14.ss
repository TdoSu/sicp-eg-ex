(load "utils.ss")

;;; count-change 计算 11 美分时的计算树, 以及时间空间复杂度

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((= kinds-of-coins 0) 0)
          (else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (cc amount 5))

(display-newline (count-change 11))

; (count-change 11)
; (cc 11 5) ==> 4
; (11 5) ==> 4
;   (11 4) ==> 4
;     (11 3) ==> 4
;       (11 2) ==> 3
;         (11 1) ==> 1
;           (11 0) --> 0
;           (10 1) ==> 1
;             (10 0) --> 0
;             (9 1) ==> 1
;               (9 0) --> 0
;               (8 1) ==> 1
;                 (8 0) --> 0
;                 (7 1) ==> 1
;                   (7 0) --> 0
;                   (6 1)  ==> 1
;                     (6 0) --> 0
;                     (5 1) ==> 1
;                       (5 0) --> 0
;                       (4 1) ==> 1
;                         (4 0) --> 0
;                         (3 1) ==> 1
;                           (3 0) --> 0
;                           (2 1) ==> 1
;                             (2 0) --> 0
;                             (1 1) ==> 1
;                               (1 0) --> 0
;                               (0 1) --> 1
;         (6 2) ==> 2
;           (6 1) ==> 1
;             (6 0) --> 0
;             (5 1) ==> 1
;               (5 0) --> 0
;               (4 1) ==> 1
;                 (4 0) --> 0
;                 (3 1) ==> 1
;                   (3 0) --> 0
;                   (2 1) ==> 1
;                     (2 0) --> 0
;                     (1 1) ==> 1
;                       (1 0) --> 0
;                       (0 1) --> 1
;           (1 2) ==> 1
;             (1 1) ==> 1
;               (1 0) --> 0
;               (0 1) --> 1
;             (-4 2) --> 0
;       (1 3) ==> 1
;         (1 2) ==> 1
;           (1 1) ==> 1
;             (1 0) --> 0
;             (0 1) --> 1
;           (-4 2) --> 0
;         (-9 3) --> 0
;     (-14 4) --> 0
;   (-39 5) --> 0

;;; 空间增长 O(n)
;;; 时间增长 O(a^n)

(exit)

