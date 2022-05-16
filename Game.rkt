(#%require (only racket error random))
(#%require "Graphics.rkt")
(load "PositionADT.rkt")
(load "MovingObjectADT.rkt")
(load "StationaryObjectADT.rkt")
(load "LevelADT.rkt")
(load "GameADT.rkt")
(load "DrawADT.rkt")
(load "VisualADT.rkt")
(load "HelpProcedures.rkt")
(load "Constants.rkt")
(load "Level-1.rkt")
(load "Level-2.rkt")
(load "Level-3.rkt")

;;
;; STARTING GAME
;;

(define game (make-game level-1 level-2 level-3))
(game 'start!)