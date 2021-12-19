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

(define level (make-level 1 (make-position 10 3)))

;;
;; Walls
;;

;; Left wall
((level 'add-wall) (make-position 0 3)) ;; Top left
((level 'add-wall) (make-position 0 4))
((level 'add-wall) (make-position 0 5))
((level 'add-wall) (make-position 0 6))
((level 'add-wall) (make-position 0 7))
((level 'add-wall) (make-position 0 8))
((level 'add-wall) (make-position 0 9))
((level 'add-wall) (make-position 0 10))
((level 'add-wall) (make-position 0 11))
((level 'add-wall) (make-position 0 12)) ;; Bottom left

((level 'add-wall) (make-position 1 3))
((level 'add-wall) (make-position 2 3))
((level 'add-wall) (make-position 3 3))
((level 'add-wall) (make-position 4 3))
((level 'add-wall) (make-position 5 3))
((level 'add-wall) (make-position 6 3))
((level 'add-wall) (make-position 7 3))
((level 'add-wall) (make-position 8 3))
((level 'add-wall) (make-position 9 3))

((level 'add-wall) (make-position 11 3))
((level 'add-wall) (make-position 12 3))
((level 'add-wall) (make-position 13 3))
((level 'add-wall) (make-position 14 3))
((level 'add-wall) (make-position 15 3))
((level 'add-wall) (make-position 16 3))
((level 'add-wall) (make-position 17 3))
((level 'add-wall) (make-position 18 3)) ;; Top right

((level 'add-wall) (make-position 18 4))
((level 'add-wall) (make-position 18 5))
((level 'add-wall) (make-position 18 6))
((level 'add-wall) (make-position 18 7))
((level 'add-wall) (make-position 18 8))
((level 'add-wall) (make-position 18 9))
((level 'add-wall) (make-position 18 10))
((level 'add-wall) (make-position 18 11))
((level 'add-wall) (make-position 18 12)) ;; Bottom right

((level 'add-wall) (make-position 2 12))
((level 'add-wall) (make-position 3 12))
((level 'add-wall) (make-position 4 12))
((level 'add-wall) (make-position 5 12))
((level 'add-wall) (make-position 6 12))
((level 'add-wall) (make-position 7 12))
((level 'add-wall) (make-position 8 12))
((level 'add-wall) (make-position 9 12))
((level 'add-wall) (make-position 10 12))
((level 'add-wall) (make-position 11 12))
((level 'add-wall) (make-position 12 12))
((level 'add-wall) (make-position 13 12))
((level 'add-wall) (make-position 14 12))
((level 'add-wall) (make-position 15 12))
((level 'add-wall) (make-position 16 12))
((level 'add-wall) (make-position 17 12))

;; First row
((level 'add-wall) (make-position 11 4))
((level 'add-wall) (make-position 16 4))
((level 'add-wall) (make-position 17 4))

;; Second row
((level 'add-wall) (make-position 2 5))
((level 'add-wall) (make-position 3 5))
((level 'add-wall) (make-position 4 5))
((level 'add-wall) (make-position 5 5))
((level 'add-wall) (make-position 6 5))
((level 'add-wall) (make-position 7 5))
((level 'add-wall) (make-position 8 5))
((level 'add-wall) (make-position 9 5))
((level 'add-wall) (make-position 13 5))
((level 'add-wall) (make-position 14 5))

;; Third row
((level 'add-wall) (make-position 5 6))
((level 'add-wall) (make-position 9 6))
;((level 'add-wall) (make-position 10 6)) ;; Needs to become door
((level 'add-wall) (make-position 11 6))
((level 'add-wall) (make-position 14 6))
((level 'add-wall) (make-position 16 6))

;; Fourth row
((level 'add-wall) (make-position 5 7))
((level 'add-wall) (make-position 11 7))
((level 'add-wall) (make-position 12 7))
((level 'add-wall) (make-position 14 7))
((level 'add-wall) (make-position 16 7))

;; Fifth row
((level 'add-wall) (make-position 5 8))
((level 'add-wall) (make-position 7 8))
((level 'add-wall) (make-position 8 8))
((level 'add-wall) (make-position 9 8))
((level 'add-wall) (make-position 10 8))
((level 'add-wall) (make-position 11 8))
((level 'add-wall) (make-position 12 8))

;; Sixth row
((level 'add-wall) (make-position 1 9))
((level 'add-wall) (make-position 2 9))
((level 'add-wall) (make-position 3 9))
((level 'add-wall) (make-position 4 9))
((level 'add-wall) (make-position 5 9))
((level 'add-wall) (make-position 7 9))
((level 'add-wall) (make-position 12 9))
((level 'add-wall) (make-position 13 9))
((level 'add-wall) (make-position 14 9))
((level 'add-wall) (make-position 15 9))
((level 'add-wall) (make-position 16 9))
((level 'add-wall) (make-position 17 9))

;; Seventh row
;((level 'add-wall) (make-position 7 10)) ;; Needs to become door
((level 'add-wall) (make-position 16 10))

;; Eighth row
((level 'add-wall) (make-position 2 11))
((level 'add-wall) (make-position 3 11))
((level 'add-wall) (make-position 4 11))
((level 'add-wall) (make-position 5 11))
((level 'add-wall) (make-position 6 11))
((level 'add-wall) (make-position 7 11))
((level 'add-wall) (make-position 12 11))
((level 'add-wall) (make-position 13 11))

;;
;; Eggs
;;

((level 'add-egg) (make-position 2 8))
((level 'add-egg) (make-position 3 8))
((level 'add-egg) (make-position 4 8))

((level 'add-scorpion) (make-position 8 10))
((level 'add-scorpion) (make-position 2 4))


(define game (make-game level))
(game 'start!)