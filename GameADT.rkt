(load "PositionADT.rkt")
(load "MovingObjectADT.rkt")
(load "EggADT.rkt")
(load "WallADT.rkt")
(load "LevelADT.rkt")
(#%require (only racket error))

(define (make-game)
  
  (define (start!)
    (display "start game"))
  
  (define (dispatch m)
    (cond
      ((eq? m 'start!) (start!))
      (else (error "ERROR in DISPATCH: Wrong message!"))))
  dispatch)