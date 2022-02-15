(#%require (only racket error))
(#%require "Graphics.rkt")
(load "PositionADT.rkt")
(load "MovingObjectADT.rkt")
(load "EggADT.rkt")
(load "WallADT.rkt")
(load "LevelADT.rkt")
(load "GameADT.rkt")
(load "DrawADT.rkt")
(load "VisualADT.rkt")
(load "HelpProcedures.rkt")
(load "Constants.rkt")

;;
;; Level initialization
;;

(define level (make-level 1 (make-position 10 3)))

;;
;; Walls
;;

((level 'add-wall) (make-position 0 0) (make-position 0 9))
((level 'add-wall) (make-position 1 0) (make-position 9 0))
((level 'add-wall) (make-position 11 0) (make-position 11 1))
((level 'add-wall) (make-position 12 0) (make-position 18 0))
((level 'add-wall) (make-position 16 1) (make-position 17 1))
((level 'add-wall) (make-position 18 1) (make-position 18 9))
((level 'add-wall) (make-position 2 9) (make-position 17 9))
((level 'add-wall) (make-position 2 2) (make-position 9 2))
((level 'add-wall) (make-position 13 2) (make-position 14 2))
((level 'add-wall) (make-position 5 3) (make-position 5 6))
((level 'add-wall) (make-position 9 3) (make-position 9 3))
((level 'add-wall) (make-position 11 3) (make-position 11 3))
((level 'add-wall) (make-position 14 3) (make-position 14 4))
((level 'add-wall) (make-position 16 3) (make-position 16 4))
((level 'add-wall) (make-position 11 4) (make-position 12 4))
((level 'add-wall) (make-position 7 5) (make-position 12 5))
((level 'add-wall) (make-position 1 6) (make-position 4 6))
((level 'add-wall) (make-position 12 6) (make-position 17 6))
((level 'add-wall) (make-position 2 8) (make-position 7 8))
((level 'add-wall) (make-position 12 8) (make-position 13 8))
((level 'add-wall) (make-position 7 6) (make-position 7 6))
((level 'add-wall) (make-position 16 7) (make-position 16 7))

;;
;; Eggs
;;

((level 'add-egg) (make-position 2 8))
((level 'add-egg) (make-position 3 8))
((level 'add-egg) (make-position 4 8))

;;
;; Scorpions
;;

((level 'add-scorpion) (make-position 8 10) 'right)
((level 'add-scorpion) (make-position 4 6) 'down)

;;
;; Starting Game
;;

(define game (make-game level))
(game 'start!)