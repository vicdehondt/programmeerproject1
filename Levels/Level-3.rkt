(define level-3 (make-level 3 (make-position 17 0) 0 12 (make-position 17 12)))

;;
;; WALLS
;;

(level-3 'add-walls '(((0 0) (0 12))
                      ((1 12) (16 12))
                      ((18 0) (18 12))
                      ((1 0) (16 0))

                      ((16 2) (17 2))
                      ((14 3) (14 3))
                      ((14 4) (16 4))

                      ((12 1) (14 1))

                      ((12 2) (12 6))
                      ((9 1) (9 8))

                      ((13 6) (16 6))

                      ((10 8) (16 8))

                      ((10 10) (16 10))
                      ((9 10) (9 11))

                      ((5 2) (7 2))
                      ((5 4) (7 4))
                      ((5 5) (7 5))
                      ((5 3) (5 7))
                      ((6 7) (8 7))

                      ((2 10) (7 10))
                      ((1 1) (1 5))
                      ((3 5) (4 5))
                      ))

;;
;; EGGS
;;

(level-3 'add-eggs '((3 4)
                     (4 4)))

;;
;; SCORPIONS
;;

(level-3 'add-scorpions '((14 7 right normal)))

;;
;; PUZZLE OBJECTS
;;

(level-3 'add-puzzleobjects '((16 7 door)
                              (6 6 key)))