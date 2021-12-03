(load "PositionADT.rkt")
(load "MovingObjectADT.rkt")
(#%require (only racket error))

(define (make-wall position-object length orientation)
  (let* ((width 6)
        (min-x-no-zone (position-object 'x))
        (min-y-no-zone (position-object 'y))
        (max-x-no-zone (cond
                        ((eq? orientation 'vertical) (+ (position-object 'x) width))
                        ((eq? orientation 'horizontal) (+ (position-object 'x) length))))
        (max-y-no-zone (cond
                         ((eq? orientation 'vertical) (+ (position-object 'y) length))
                         ((eq? orientation 'horizontal) (+ (position-object 'y) width)))))
    
    (define (free? position-object)
      (or (> (position-object 'x) max-x-no-zone)
           (< (position-object 'x) min-x-no-zone)
           (> (position-object 'y) max-y-no-zone)
           (< (position-object 'y) min-y-no-zone)))
  
    (define (dispatch m)
      (cond
        ((eq? m 'free?) free?)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))