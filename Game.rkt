(#%require (only racket error))
(load "PositionADT.rkt")
(load "MovingObjectADT.rkt")
(load "EggADT.rkt")
(load "WallADT.rkt")
(load "LevelADT.rkt")
(load "GameADT.rkt")
(load "DrawADT.rkt")


(define game (make-draw))
(game 'start!)