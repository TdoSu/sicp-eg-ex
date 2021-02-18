(load "utils.ss")

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (v1-frame frame)
  (let ((o (origin frame))
        (e1 (edge1 frame))
        (e2 (edge2 frame)))
    o))

(define (v2-frame frame)
  (let ((o (origin frame))
        (e1 (edge1 frame))
        (e2 (edge2 frame)))
    (add-vector o e1)))

(define (v3-frame frame)
  (let ((o (origin frame))
        (e1 (edge1 frame))
        (e2 (edge2 frame)))
    (add-vector o (add-vector e1 e2))))

(define (v4-frame frame)
  (let ((o (origin frame))
        (e1 (edge1 frame))
        (e2 (edge2 frame)))
    (add-vector o e2)))

(define (frame-painter frame)
  (let ((v1 (v1-frame frame))
        (v2 (v2-frame frame))
        (v3 (v3-frame frame))
        (v4 (v4-frame frame)))
    (let ((segment-list
            (list (make-segment v1 v2)
                  (make-segment v2 v3)
                  (make-segment v3 v4)
                  (make-segment v4 v1))))
      ((segments->painter segment-list) frame))))

(define (X-painter frame)
  (let ((v1 (v1-frame frame))
        (v2 (v2-frame frame))
        (v3 (v3-frame frame))
        (v4 (v4-frame frame)))
    (let ((segment-list
            (list (make-segment v1 v3)
                  (make-segment v4 v2))))
      ((segments->painter segment-list) frame))))

(define (average-vector v1 v2)
  (scale-vector 1/2 (add-vector v1 v2)))

(define (midpoint-painter frame)
  (let ((v1 (v1-frame frame))
        (v2 (v2-frame frame))
        (v3 (v3-frame frame))
        (v4 (v4-frame frame)))
    (let ((mv1 (average-vector v1 v2))
          (mv2 (average-vector v2 v3))
          (mv3 (average-vector v3 v4))
          (mv4 (average-vector v5 v1)))
      (let ((segment-list
              (list (make-segment mv1 mv2)
                    (make-segment mv2 mv3)
                    (make-segment mv3 mv4)
                    (make-segment mv4 mv1))))
        ((segments->painter segment-list) frame)))))

;;; TODO: wave

(exit)

