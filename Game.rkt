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

(define level (make-level 1 (make-position 10 0)))

;;
;; Walls
;;

;; Left wall
;#|
((level 'add-wall) (make-position 0 0)) ;; Top left
((level 'add-wall) (make-position 0 1))
((level 'add-wall) (make-position 0 2))
((level 'add-wall) (make-position 0 3))
((level 'add-wall) (make-position 0 4))
((level 'add-wall) (make-position 0 5))
((level 'add-wall) (make-position 0 6))
((level 'add-wall) (make-position 0 7))
((level 'add-wall) (make-position 0 8))
((level 'add-wall) (make-position 0 9)) ;; Bottom left
;|#
#|
((level 'add-wall) (make-position 0 0) (make-position 0 9) 'vertical)
|#
;; Top wall
;#|
((level 'add-wall) (make-position 1 0))
((level 'add-wall) (make-position 2 0))
((level 'add-wall) (make-position 3 0))
((level 'add-wall) (make-position 4 0))
((level 'add-wall) (make-position 5 0))
((level 'add-wall) (make-position 6 0))
((level 'add-wall) (make-position 7 0))
((level 'add-wall) (make-position 8 0))
((level 'add-wall) (make-position 9 0))
;|#
#|
((level 'add-wall) (make-position 1 0) (make-position 9 0) 'horizontal)
|#
;; Starting point
;#|
((level 'add-wall) (make-position 11 0))
((level 'add-wall) (make-position 12 0))
((level 'add-wall) (make-position 13 0))
((level 'add-wall) (make-position 14 0))
((level 'add-wall) (make-position 15 0))
((level 'add-wall) (make-position 16 0))
((level 'add-wall) (make-position 17 0))
((level 'add-wall) (make-position 18 0)) ;; Top right
;|#
#|
((level 'add-wall) (make-position 18 0) (make-position 11 0) 'horizontal)
|#
;; Right wall
;#|
((level 'add-wall) (make-position 18 1))
((level 'add-wall) (make-position 18 2))
((level 'add-wall) (make-position 18 3))
((level 'add-wall) (make-position 18 4))
((level 'add-wall) (make-position 18 5))
((level 'add-wall) (make-position 18 6))
((level 'add-wall) (make-position 18 7))
((level 'add-wall) (make-position 18 8))
((level 'add-wall) (make-position 18 9)) ;; Bottom right
;|#
#|
((level 'add-wall) (make-position 18 1) (make-position 18 9) 'vertical)
|#
;; Bottom wall
;; Exit point
;#|
((level 'add-wall) (make-position 2 9))
((level 'add-wall) (make-position 3 9))
((level 'add-wall) (make-position 4 9))
((level 'add-wall) (make-position 5 9))
((level 'add-wall) (make-position 6 9))
((level 'add-wall) (make-position 7 9))
((level 'add-wall) (make-position 8 9))
((level 'add-wall) (make-position 9 9))
((level 'add-wall) (make-position 10 9))
((level 'add-wall) (make-position 11 9))
((level 'add-wall) (make-position 12 9))
((level 'add-wall) (make-position 13 9))
((level 'add-wall) (make-position 14 9))
((level 'add-wall) (make-position 15 9))
((level 'add-wall) (make-position 16 9))
((level 'add-wall) (make-position 17 9))
;|#
#|
((level 'add-wall) (make-position 2 9) (make-position 17 9) 'horizontal)
|#
;; Inside walls:

#|
((level 'add-wall) (make-position 11 1) (make-position 11 1) 'horizontal)
((level 'add-wall) (make-position 16 1) (make-position 17 1) 'horizontal)

((level 'add-wall) (make-position 2 2) (make-position 9 2) 'horizontal)
((level 'add-wall) (make-position 13 2) (make-position 14 2) 'horizontal)

((level 'add-wall) (make-position 5 3) (make-position 5 6) 'vertical)
((level 'add-wall) (make-position 9 3) (make-position 11 3) 'horizontal)
((level 'add-wall) (make-position 14 3) (make-position 14 4) 'vertical)
((level 'add-wall) (make-position 16 3) (make-position 16 4) 'vertical)

((level 'add-wall) (make-position 11 4) (make-position 11 5) 'vertical)
((level 'add-wall) (make-position 12 4) (make-position 12 6) 'vertical)

((level 'add-wall) (make-position 7 5) (make-position 10 5) 'horizontal)

((level 'add-wall) (make-position 1 6) (make-position 4 6) 'horizontal)
((level 'add-wall) (make-position 7 6) (make-position 7 8) 'vertical)
((level 'add-wall) (make-position 13 6) (make-position 17 6) 'horizontal)

((level 'add-wall) (make-position 16 7) (make-position 16 7) 'horizontal)

((level 'add-wall) (make-position 2 8) (make-position 6 8) 'horizontal)
((level 'add-wall) (make-position 12 8) (make-position 13 8) 'horizontal)
|#
;#|
;; First row
((level 'add-wall) (make-position 11 1))
((level 'add-wall) (make-position 16 1))
((level 'add-wall) (make-position 17 1))

;; Second row
((level 'add-wall) (make-position 2 2))
((level 'add-wall) (make-position 3 2))
((level 'add-wall) (make-position 4 2))
((level 'add-wall) (make-position 5 2))
((level 'add-wall) (make-position 6 2))
((level 'add-wall) (make-position 7 2))
((level 'add-wall) (make-position 8 2))
((level 'add-wall) (make-position 9 2))
((level 'add-wall) (make-position 13 2))
((level 'add-wall) (make-position 14 2))

;; Third row
((level 'add-wall) (make-position 5 3))
((level 'add-wall) (make-position 9 3))
((level 'add-wall) (make-position 10 3)) ;; Needs to become door
((level 'add-wall) (make-position 11 3))
((level 'add-wall) (make-position 14 3))
((level 'add-wall) (make-position 16 3))

;; Fourth row
((level 'add-wall) (make-position 5 4))
((level 'add-wall) (make-position 11 4))
((level 'add-wall) (make-position 12 4))
((level 'add-wall) (make-position 14 4))
((level 'add-wall) (make-position 16 4))

;; Fifth row
((level 'add-wall) (make-position 5 5))
((level 'add-wall) (make-position 7 5))
((level 'add-wall) (make-position 8 5))
((level 'add-wall) (make-position 9 5))
((level 'add-wall) (make-position 10 5))
((level 'add-wall) (make-position 11 5))
((level 'add-wall) (make-position 12 5))

;; Sixth row
((level 'add-wall) (make-position 1 6))
((level 'add-wall) (make-position 2 6))
((level 'add-wall) (make-position 3 6))
((level 'add-wall) (make-position 4 6))
((level 'add-wall) (make-position 5 6))
((level 'add-wall) (make-position 7 6))
((level 'add-wall) (make-position 12 6))
((level 'add-wall) (make-position 13 6))
((level 'add-wall) (make-position 14 6))
((level 'add-wall) (make-position 15 6))
((level 'add-wall) (make-position 16 6))
((level 'add-wall) (make-position 17 6))

;; Seventh row
((level 'add-wall) (make-position 7 7)) ;; Needs to become door
((level 'add-wall) (make-position 16 7))

;; Eighth row
((level 'add-wall) (make-position 2 8))
((level 'add-wall) (make-position 3 8))
((level 'add-wall) (make-position 4 8))
((level 'add-wall) (make-position 5 8))
((level 'add-wall) (make-position 6 8))
((level 'add-wall) (make-position 7 8))
((level 'add-wall) (make-position 12 8))
((level 'add-wall) (make-position 13 8))
;|#
;;
;; Eggs
;;

((level 'add-egg) (make-position 2 5))


(define game (make-game level))
(game 'start!)