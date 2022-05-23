(define level-1 (make-level (make-position 10 3) 3 13 (make-position 1 12)))

;;
;; WALLS
;;

(level-1 'add-walls '(((0 3) (0 12))
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

(level-1 'add-eggs '((1 8)
                     (2 8)
                     (3 8)))

;;
;; SCORPIONS
;;

(level-1 'add-scorpions '((8 10 right normal)
                          (4 8 left random)
                          (17 4 up random)))

;;
;; PUZZLE OBJECTS
;;


(level-1 'add-puzzle-objects '((13 8 key)
                               (4 8 key)
                               (17 10 bomb)
                               (10 6 door)
                               (6 9 door)
                               (1 12 weak-wall)))

(level-1 'add-powerups '((1 7 food)))

