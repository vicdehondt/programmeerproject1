(#%require (only racket error))
(#%require "Graphics.rkt")
(load "PositionADT.rkt")
(load "MovingObjectADT.rkt")
(load "EggADT.rkt")
(load "WallADT.rkt")
(load "PowerupADT.rkt")
(load "PuzzleObjectADT.rkt")
(load "LevelADT.rkt")
(load "GameADT.rkt")
(load "DrawADT.rkt")
(load "VisualADT.rkt")
(load "HelpProcedures.rkt")
(load "Constants.rkt")

;;
;; LEVEL INITIALIZATION
;;

(define level (make-level 1 (make-position 10 3)))

;;
;; WALLS
;;

(level 'add-walls '(((0 0) (0 9))
                    ((1 0) (9 0))
                    ((11 0) (11 1))
                    ((12 0) (18 0))
                    ((16 1) (17 1))
                    ((18 1) (18 9))
                    ((2 9) (17 9))
                    ((2 2) (9 2))
                    ((13 2) (14 2))
                    ((5 3) (5 6))
                    ((9 3) (9 3))
                    ((11 3) (11 3))
                    ((14 3) (14 4))
                    ((16 3) (16 4))
                    ((11 4) (12 4))
                    ((7 5) (12 5))
                    ((1 6) (4 6))
                    ((12 6) (17 6))
                    ((2 8) (7 8))
                    ((12 8) (13 8))
                    ((7 6) (7 6))
                    ((16 7) (16 7))))

;;
;; EGGS
;;

(level 'add-eggs '((2 8)
                   (3 8)
                   (4 8)
                   (17 10)
                   (13 8)))

;;
;; SCORPIONS
;;

(level 'add-scorpions '((8 10 right)
                        (4 6 down)))

;;
;; STARTING GAME
;;

(define game (make-game level))
(game 'start!)