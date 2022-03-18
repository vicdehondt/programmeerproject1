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

(define level (make-level 1 (make-position 10 3) 3 12))

;;
;; WALLS
;;

(level 'add-walls '(((0 3) (0 12))
                    ((1 3) (9 3))
                    ((11 3) (11 4))
                    ((12 3) (18 3))
                    ((16 4) (17 4))
                    ((18 4) (18 12))
                    ((2 12) (17 12))
                    ((2 5) (9 5))
                    ((13 5) (14 5))
                    ((5 6) (5 9))
                    ((9 6) (9 6))
                    ((11 6) (11 6))
                    ((14 6) (14 7))
                    ((16 6) (16 7))
                    ((11 7) (12 7))
                    ((7 8) (12 8))
                    ((1 9) (4 9))
                    ((12 9) (17 9))
                    ((2 11) (7 11))
                    ((12 11) (13 11))
                    ((7 9) (7 9))
                    ((16 10) (16 10))))

;;
;; EGGS
;;

(level 'add-eggs '((2 8)
                   (3 8)
                   (4 8)))

;;
;; SCORPIONS
;;

(level 'add-scorpions '((8 10 right)
                        (4 6 down)))

;;
;; PUZZLE OBJECTS
;;

(level 'add-puzzleobjects '((13 8 key)
                            (17 10 key)
                            (10 6 door)
                            (7 10 door)))

;;
;; STARTING GAME
;;

(define game (make-game level))
(game 'start!)