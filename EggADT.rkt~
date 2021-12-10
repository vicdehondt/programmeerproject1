(load "PositionADT.rkt")
(#%require (only racket error))

(define (make-egg position-object)
  (let ((position position-object))
    
    (define (dispatch m)
      (cond
        ((eq? m 'position) position)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))