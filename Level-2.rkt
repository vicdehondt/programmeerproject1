(define level-2 (make-level (make-position 1 0) 0 12 (make-position 17 12)))

;;
;; WALLS
;;

(level-2 'add-walls '(((0 0) (0 12))
                      ((1 12) (16 12))
                      ((18 0) (18 12))
                      ((2 0) (17 0))

                      ((2 2) (16 2))
                      ((2 4) (17 4))

                      ((2 5) (2 9))
                      ((6 5) (6 9))
                      ((4 8) (4 9))
                      ((5 8) (5 9))

                      ((1 11) (6 11))
                      ((8 10) (15 10))

                      ((8 8) (12 8))
                      ((11 5) (11 7))
                      ((12 5) (12 7))
                      ((16 8) (16 11))
                      ((15 8) (15 9))
                      ((14 8) (14 9))))

;;
;; EGGS
;;

(level-2 'add-eggs '((3 5)
                     (4 5)
                     (5 5)))

;;
;; SCORPIONS
;;

(level-2 'add-scorpions '((14 7 right normal)))

;;
;; PUZZLE OBJECTS
;;

(level-2 'add-puzzle-objects '((15 11 key)
                              (17 2 key)
                              (7 8 door)
                              (17 12 door)))

;;
;; POWER-UPS
;;

(level-2 'add-powerups '((10 7 shield)))