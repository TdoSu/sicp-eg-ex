(load "utils.ss")

;;; 二叉树集合的 lookup

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((let ((l (lookup given-key (left-branch set-of-records))))
           (if (not l)
               (lookup given-key (right-branch set-of-records))
               l)))))

(exit)

