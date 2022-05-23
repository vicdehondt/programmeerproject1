(define (make-visual window)
  (let ((bomb-animation-time 0)
        (speed-up? #f)
        (shield? #f)
        (ant-sequence (make-tile-sequence (list (make-bitmap-tile "images/48px/FireAnt-Up.png" "images/48px/FireAnt-Up-mask.png")
                                                (make-bitmap-tile "images/48px/FireAnt-Right.png" "images/48px/FireAnt-Right-mask.png")
                                                (make-bitmap-tile "images/48px/FireAnt-Down.png" "images/48px/FireAnt-Down-mask.png")
                                                (make-bitmap-tile "images/48px/FireAnt-Left.png" "images/48px/FireAnt-Left-mask.png"))))
        (highscore-sequence (make-tile-sequence (list (make-bitmap-tile "images/Numbers/0.png" "images/Numbers/0-mask.png")
                                                      (make-bitmap-tile "images/Numbers/1.png" "images/Numbers/1-mask.png")
                                                      (make-bitmap-tile "images/Numbers/2.png" "images/Numbers/2-mask.png")
                                                      (make-bitmap-tile "images/Numbers/3.png" "images/Numbers/3-mask.png")
                                                      (make-bitmap-tile "images/Numbers/4.png" "images/Numbers/4-mask.png")
                                                      (make-bitmap-tile "images/Numbers/5.png" "images/Numbers/5-mask.png")
                                                      (make-bitmap-tile "images/Numbers/6.png" "images/Numbers/6-mask.png")
                                                      (make-bitmap-tile "images/Numbers/7.png" "images/Numbers/7-mask.png")
                                                      (make-bitmap-tile "images/Numbers/8.png" "images/Numbers/8-mask.png")
                                                      (make-bitmap-tile "images/Numbers/9.png" "images/Numbers/9-mask.png"))))
        (score-text-tile (make-bitmap-tile "images/Score/Score.png" "images/Score/Score-mask.png"))
        (sky-tile (make-bitmap-tile "images/Sky/Sky.png"))
        (highscore-text-tile (make-bitmap-tile "images/Highscore/Highscore.png" "images/Highscore/Highscore-mask.png"))
        (lives-text-tile (make-bitmap-tile "images/Lives/Lives.png" "images/Lives/Lives-mask.png"))
        (score (make-vector score-size))
        (highscore (make-vector score-size))
        (lives-sequence (make-tile-sequence (list (make-bitmap-tile "images/Lives/0.png" "images/Numbers/0-mask.png")
                                                  (make-bitmap-tile "images/Lives/1.png" "images/Numbers/1-mask.png")
                                                  (make-bitmap-tile "images/Lives/2.png" "images/Numbers/2-mask.png")
                                                  (make-bitmap-tile "images/Lives/3.png" "images/Numbers/3-mask.png")
                                                  (make-bitmap-tile "images/Lives/4.png" "images/Numbers/4-mask.png")
                                                  (make-bitmap-tile "images/Lives/5.png" "images/Numbers/5-mask.png")
                                                  (make-bitmap-tile "images/Lives/6.png" "images/Numbers/6-mask.png")
                                                  (make-bitmap-tile "images/Lives/7.png" "images/Numbers/7-mask.png")
                                                  (make-bitmap-tile "images/Lives/8.png" "images/Numbers/8-mask.png")
                                                  (make-bitmap-tile "images/Lives/9.png" "images/Numbers/9-mask.png"))))
        (wall-tiles '())
        (egg-tiles '())
        (key-tiles '())
        (bomb-tiles '())
        (door-tiles '())
        (weak-wall-tiles '())
        (shield-shroom-tiles '())
        (food-tiles '())
        (normal-scorpion-tiles '())
        (random-scorpion-tiles '()))

    (define (clear-tiles!)
      (set! wall-tiles '())
      (set! egg-tiles '())
      (set! key-tiles '())
      (set! bomb-tiles '())
      (set! door-tiles '())
      (set! weak-wall-tiles '())
      (set! shield-shroom-tiles '())
      (set! food-tiles '())
      (set! normal-scorpion-tiles '())
      (set! random-scorpion-tiles '()))
    
    ;;
    ;; LAYER INITIALIZATION
    ;;
    
    (define base-layer (window 'make-layer))
    (define game-objects-layer (window 'make-layer))

    ;;
    ;; STATIC TILE VISUALISATION
    ;;

    ((score-text-tile 'set-x!) score-text-x)
    ((score-text-tile 'set-y!) score-text-y)

    ((highscore-text-tile 'set-x!) highscore-text-x)
    ((highscore-text-tile 'set-y!) highscore-text-y)

    ((lives-text-tile 'set-x!) lives-text-x)
    ((lives-text-tile 'set-y!) lives-text-y)

    ((lives-sequence 'set-x!) lives-sequence-x)
    ((lives-sequence 'set-y!) lives-sequence-y)

    ((sky-tile 'set-x!) sky-x)
    ((sky-tile 'set-y!) sky-y)

    ;;
    ;; DRAW PROCEDURES
    ;;
    
    ; Show the highscore on screen
    (define (draw-highscore! game-highscore)
      (let loop ((count 0))
        (if (<= count (- score-size 1))
            (begin
              (((vector-ref highscore count) 'set-current!) (vector-ref game-highscore count))
              (loop (+ count 1))))))

    (define (draw-score! game-score)
      (let loop ((count 0))
        (if (<= count (- score-size 1))
            (begin
              (((vector-ref score count) 'set-current!) (vector-ref game-score count))
              (loop (+ count 1))))))

    ; Show lives remaining on screen
    (define (draw-lives! game)
      (let ((lives (game 'lives)))
        ((lives-sequence 'set-current!) lives)))

    ;; Lets the tile look to the direction it's going
    (define (set-current-tile! orientation sequence object)
      (define (set-tile! index)
        (cond
          ((eq? orientation 'up) ((sequence 'set-current!) (+ up-index index)))
          ((eq? orientation 'right) ((sequence 'set-current!) (+ right-index index)))
          ((eq? orientation 'down) ((sequence 'set-current!) (+ down-index index)))
          ((eq? orientation 'left) ((sequence 'set-current!) (+ left-index index)))
          (else (error "[ERROR in VisualADT set-current-tile!] Wrong orientation!"))))
      
      (if (eq? (object 'kind) 'ant)
          (set-tile! standard-index)
          (cond
            (shield? (set-tile! shield-index))
            (speed-up? (set-tile! speed-index))
            (else (set-tile! standard-index)))))

    (define (draw-ant! level-object)
      (let ((ant (level-object 'ant)))
        (set-current-tile! (ant 'orientation) ant-sequence ant)
        (draw-object! ant ant-sequence)))

    (define (draw! level-object kind)

      (define (draw-every-stationary-piece kind)
        (for-each-object (lambda (object) (draw-stationary-piece! object)) (level-object 'give-list kind)))

      (define (draw-every-scorpion)
        (for-each-object (lambda (scorpion) (draw-scorpion-piece! scorpion)) (level-object 'give-list 'normal-scorpion))
        (for-each-object (lambda (scorpion) (draw-scorpion-piece! scorpion)) (level-object 'give-list 'random-scorpion)))
      
      (cond
        ((eq? kind 'scorpion) (draw-every-scorpion))
        (else (draw-every-stationary-piece kind))))

    (define (draw-scorpion-piece! scorpion-object)
      (let ((sequence (get-object-piece scorpion-object))
            (orientation (scorpion-object 'orientation)))
        (set-current-tile! orientation sequence scorpion-object)
        (draw-object! scorpion-object sequence)))

    (define (draw-stationary-piece! object)
      (let ((tile (get-object-piece object)))
        (draw-object! object tile)))

    ;; Looks for the right tile bound to the given object
    (define (get-object-piece object)
      (let* ((tile-list (which-tiles-list (object 'kind)))
             (result (assoc object tile-list)))
        (if result
            (cdr result)
            (add-object-piece! object))))

    (define (which-tiles-list kind)
      (cond
        ((eq? kind 'wall) wall-tiles)
        ((eq? kind 'egg) egg-tiles)
        ((eq? kind 'key) key-tiles)
        ((eq? kind 'bomb) bomb-tiles)
        ((eq? kind 'door) door-tiles)
        ((eq? kind 'weak-wall) weak-wall-tiles)
        ((eq? kind 'shield-shroom) shield-shroom-tiles)
        ((eq? kind 'food) food-tiles)
        ((eq? kind 'normal-scorpion) normal-scorpion-tiles)
        ((eq? kind 'random-scorpion) random-scorpion-tiles)
        (else (error "[ERROR in VisualADT which-tiles-list] Wrong kind!"))))

    (define (add-object-piece! object)
      (let* ((kind (object 'kind))
             (new-normal-scorpion-sequence (make-tile-sequence (list (make-bitmap-tile "images/48px/Normal-Scorpion-Up.png" "images/48px/Scorpion-Up-mask.png")
                                                                     (make-bitmap-tile "images/48px/Normal-Scorpion-Right.png" "images/48px/Scorpion-Right-mask.png")
                                                                     (make-bitmap-tile "images/48px/Normal-Scorpion-Down.png" "images/48px/Scorpion-Down-mask.png")
                                                                     (make-bitmap-tile "images/48px/Normal-Scorpion-Left.png" "images/48px/Scorpion-Left-mask.png")
                                                                     (make-bitmap-tile "images/48px/Speed-scorpion-up.png" "images/48px/Scorpion-Up-mask.png")
                                                                     (make-bitmap-tile "images/48px/Speed-scorpion-right.png" "images/48px/Scorpion-Right-mask.png")
                                                                     (make-bitmap-tile "images/48px/Speed-scorpion-down.png" "images/48px/Scorpion-Down-mask.png")
                                                                     (make-bitmap-tile "images/48px/Speed-scorpion-left.png" "images/48px/Scorpion-Left-mask.png")
                                                                     (make-bitmap-tile "images/48px/Shield-scorpion-up.png" "images/48px/Scorpion-Up-mask.png")
                                                                     (make-bitmap-tile "images/48px/Shield-scorpion-right.png" "images/48px/Scorpion-Right-mask.png")
                                                                     (make-bitmap-tile "images/48px/Shield-scorpion-down.png" "images/48px/Scorpion-Down-mask.png")
                                                                     (make-bitmap-tile "images/48px/Shield-scorpion-left.png" "images/48px/Scorpion-Left-mask.png"))))
             (new-random-scorpion-sequence (make-tile-sequence (list (make-bitmap-tile "images/48px/Random-Scorpion-Up.png" "images/48px/Scorpion-Up-mask.png")
                                                                     (make-bitmap-tile "images/48px/Random-Scorpion-Right.png" "images/48px/Scorpion-Right-mask.png")
                                                                     (make-bitmap-tile "images/48px/Random-Scorpion-Down.png" "images/48px/Scorpion-Down-mask.png")
                                                                     (make-bitmap-tile "images/48px/Random-Scorpion-Left.png" "images/48px/Scorpion-Left-mask.png")
                                                                     (make-bitmap-tile "images/48px/Speed-scorpion-up.png" "images/48px/Scorpion-Up-mask.png")
                                                                     (make-bitmap-tile "images/48px/Speed-scorpion-right.png" "images/48px/Scorpion-Right-mask.png")
                                                                     (make-bitmap-tile "images/48px/Speed-scorpion-down.png" "images/48px/Scorpion-Down-mask.png")
                                                                     (make-bitmap-tile "images/48px/Speed-scorpion-left.png" "images/48px/Scorpion-Left-mask.png")
                                                                     (make-bitmap-tile "images/48px/Shield-scorpion-up.png" "images/48px/Scorpion-Up-mask.png")
                                                                     (make-bitmap-tile "images/48px/Shield-scorpion-right.png" "images/48px/Scorpion-Right-mask.png")
                                                                     (make-bitmap-tile "images/48px/Shield-scorpion-down.png" "images/48px/Scorpion-Down-mask.png")
                                                                     (make-bitmap-tile "images/48px/Shield-scorpion-left.png" "images/48px/Scorpion-Left-mask.png"))))
             (new-egg-tile (make-bitmap-tile "images/48px/Egg.png" "images/48px/Egg-mask.png"))
             (new-shield-shroom-tile (make-bitmap-tile "images/48px/champignon.png" "images/48px/champignon-mask.png"))
             (new-food-tile (vector-ref (vector (make-bitmap-tile "images/48px/druif.png" "images/48px/druif-mask.png")
                                                (make-bitmap-tile "images/48px/banaan.png" "images/48px/banaan-mask.png")
                                                (make-bitmap-tile "images/48px/Food.png" "images/48px/Food-mask.png"))
                                        (- (random 1 4) 1)))
             (new-key-tile (make-bitmap-tile "images/48px/Key.png" "images/48px/Key-mask.png"))
             (new-bomb-tile (make-bitmap-tile "images/48px/Bomb.png" "images/48px/Bomb-mask.png"))
             (new-door-tile (make-tile grid-cell grid-cell "images/48px/Door.png"))
             (new-weak-wall-tile (make-bitmap-tile "images/48px/Weak-wall.png" "images/48px/Weak-wall-mask.png"))
             (new-wall-tile (make-tile grid-cell grid-cell "images/48px/Wall.png")))
        
        (define (add-piece new-tile)
          (add-object-tile! object new-tile)
          (show! new-tile kind)
          new-tile)
        
        (cond
          ((eq? kind 'normal-scorpion) (add-piece new-normal-scorpion-sequence))
          ((eq? kind 'random-scorpion) (add-piece new-random-scorpion-sequence))
          ((eq? kind 'egg) (add-piece new-egg-tile))
          ((eq? kind 'key) (add-piece new-key-tile))
          ((eq? kind 'bomb) (add-piece new-bomb-tile))
          ((eq? kind 'door) (add-piece new-door-tile))
          ((eq? kind 'weak-wall) (add-piece new-weak-wall-tile))
          ((eq? kind 'shield-shroom) (add-piece new-shield-shroom-tile))
          ((eq? kind 'food) (add-piece new-food-tile))
          ((eq? kind 'wall) (add-piece new-wall-tile))
          (else (error "[ERROR in VisualADT add-object-piece!] Wrong kind!")))))

    (define (draw-object! obj tile)
      (let* ((position (obj 'position))
             (new-x (* (position 'x) grid-cell))
             (new-y (* (position 'y) grid-cell)))
        ((tile 'set-x!) new-x)
        ((tile 'set-y!) new-y)))

    ;; Help procedure for adding an object and its tile
    (define (add-object-tile! object tile )
      (let ((kind (object 'kind)))
        (cond
          ((eq? kind 'normal-scorpion) (set! normal-scorpion-tiles (cons (cons object tile) normal-scorpion-tiles)))
          ((eq? kind 'random-scorpion) (set! random-scorpion-tiles (cons (cons object tile) random-scorpion-tiles)))
          ((eq? kind 'egg) (set! egg-tiles (cons (cons object tile) egg-tiles)))
          ((eq? kind 'key) (set! key-tiles (cons (cons object tile) key-tiles)))
          ((eq? kind 'bomb) (set! bomb-tiles (cons (cons object tile) bomb-tiles)))
          ((eq? kind 'door) (set! door-tiles (cons (cons object tile) door-tiles)))
          ((eq? kind 'weak-wall) (set! weak-wall-tiles (cons (cons object tile) weak-wall-tiles)))
          ((eq? kind 'shield-shroom) (set! shield-shroom-tiles (cons (cons object tile) shield-shroom-tiles)))
          ((eq? kind 'food) (set! food-tiles (cons (cons object tile) food-tiles)))
          ((eq? kind 'wall) (set! wall-tiles (cons (cons object tile) wall-tiles)))
          (else (error "[ERROR in VisualADT add-object-tile!] Wrong kind!")))))

    (define (show! tile kind)
      (define (show-score! count x y vector kind)
        (if (<= count (- score-size 1))
            (let ((sequence (if (eq? kind 'score) (make-tile-sequence (list (make-bitmap-tile "images/Numbers/0.png" "images/Numbers/0-mask.png")
                                                                            (make-bitmap-tile "images/Numbers/1.png" "images/Numbers/1-mask.png")
                                                                            (make-bitmap-tile "images/Numbers/2.png" "images/Numbers/2-mask.png")
                                                                            (make-bitmap-tile "images/Numbers/3.png" "images/Numbers/3-mask.png")
                                                                            (make-bitmap-tile "images/Numbers/4.png" "images/Numbers/4-mask.png")
                                                                            (make-bitmap-tile "images/Numbers/5.png" "images/Numbers/5-mask.png")
                                                                            (make-bitmap-tile "images/Numbers/6.png" "images/Numbers/6-mask.png")
                                                                            (make-bitmap-tile "images/Numbers/7.png" "images/Numbers/7-mask.png")
                                                                            (make-bitmap-tile "images/Numbers/8.png" "images/Numbers/8-mask.png")
                                                                            (make-bitmap-tile "images/Numbers/9.png" "images/Numbers/9-mask.png")))
                                (make-tile-sequence (list (make-bitmap-tile "images/Highscore/0.png" "images/Numbers/0-mask.png")
                                                          (make-bitmap-tile "images/Highscore/1.png" "images/Numbers/1-mask.png")
                                                          (make-bitmap-tile "images/Highscore/2.png" "images/Numbers/2-mask.png")
                                                          (make-bitmap-tile "images/Highscore/3.png" "images/Numbers/3-mask.png")
                                                          (make-bitmap-tile "images/Highscore/4.png" "images/Numbers/4-mask.png")
                                                          (make-bitmap-tile "images/Highscore/5.png" "images/Numbers/5-mask.png")
                                                          (make-bitmap-tile "images/Highscore/6.png" "images/Numbers/6-mask.png")
                                                          (make-bitmap-tile "images/Highscore/7.png" "images/Numbers/7-mask.png")
                                                          (make-bitmap-tile "images/Highscore/8.png" "images/Numbers/8-mask.png")
                                                          (make-bitmap-tile "images/Highscore/9.png" "images/Numbers/9-mask.png"))))))
              ((game-objects-layer 'add-drawable) sequence)
              ((sequence 'set-x!) x)
              ((sequence 'set-y!) y)
              (vector-set! vector count sequence)
              (show-score! (+ count 1) (+ x score-x-spacing) y vector kind))))
      (cond
        ((or (eq? kind 'normal-scorpion)
             (eq? kind 'random-scorpion)) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'egg) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'key) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'bomb) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'door) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'weak-wall) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'shield-shroom) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'food) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'wall) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'score) (show-score! 0 score-number-x-start score-number-y-start score 'score))
        ((eq? kind 'highscore) (show-score! 0 highscore-number-x-start highscore-number-y-start highscore 'highscore))
        ((eq? kind 'lives) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'sky) ((base-layer 'add-drawable) tile))
        (else (error "[ERROR in VisualADT show!] Wrong kind!"))))
    
    ;;
    ;; REMOVING
    ;;

    (define (which-to-remove level-object kind)

      (define (search tiles layer)
        (let ((current (car tiles))
              (remaining (cdr tiles)))
          (if (not (level-object 'member? (car current) kind))
              (begin
                ((layer 'remove-drawable) (cdr current))
                current)
              (search remaining layer))))
      
      (cond
        ((eq? kind 'egg) (search egg-tiles game-objects-layer))
        ((eq? kind 'shield-shroom) (search shield-shroom-tiles game-objects-layer))
        ((eq? kind 'food) (search food-tiles game-objects-layer))
        ((eq? kind 'key) (search key-tiles game-objects-layer))
        ((eq? kind 'bomb) (search bomb-tiles game-objects-layer))
        ((eq? kind 'door) (search door-tiles game-objects-layer))
        ((eq? kind 'weak-wall) (search weak-wall-tiles game-objects-layer))))
    
    (define (remove-from-list element lst)
      (let ((search-list (reverse lst)))
        (let search-and-remove ((current (car search-list))
                                (remaining (cdr search-list))
                                (result '()))
          (cond
            ((and (null? remaining) (eq? current element)) result)
            ((null? remaining) (cons current result))
            ((eq? current element) (append result remaining))
            (else (search-and-remove (car remaining) (cdr remaining) (cons current result)))))))

    (define (check-for-remove level-object kind)
      (define (check tiles)
        (if (> (length tiles) (level-object 'length? kind))
            (remove-from-list (which-to-remove level-object kind) tiles)
            tiles))
      (cond
        ((eq? kind 'egg) (set! egg-tiles (check egg-tiles)))
        ((eq? kind 'shield-shroom) (set! shield-shroom-tiles (check shield-shroom-tiles)))
        ((eq? kind 'food) (set! food-tiles (check food-tiles)))
        ((eq? kind 'key) (set! key-tiles (check key-tiles)))
        ((eq? kind 'bomb) (set! bomb-tiles (check bomb-tiles)))
        ((eq? kind 'door) (set! door-tiles (check door-tiles)))
        ((eq? kind 'weak-wall) (set! weak-wall-tiles (check weak-wall-tiles)))))

    (define (speed-up value)
      (if (not (eq? speed-up? value))
          (begin
            (set! speed-up? value))))

    (define (shield value)
      (if (not (eq? shield? value))
          (begin
            (set! shield? value))))

    ;;
    ;; GAME OVER
    ;;

    (define game-over-tile (make-bitmap-tile "images/GameOver.png"))
    
    (define (game-over!)
      (base-layer 'empty)
      (game-objects-layer 'empty)
      ((base-layer 'add-drawable) game-over-tile)
      (show! 'none 'score)
      (show! 'none 'highscore)
      ((base-layer 'add-drawable) score-text-tile)
      ((base-layer 'add-drawable) highscore-text-tile)
      ((press-space-tile 'set-y!) press-space-y)
      (draw-score! (game 'score))
      (draw-highscore! (game 'highscore)))

    ;;
    ;; GAME WIN
    ;;

    (define game-win-tile (make-bitmap-tile "images/Win.png"))

    (define (game-win!)
      (base-layer 'empty)
      (game-objects-layer 'empty)
      ((base-layer 'add-drawable) game-win-tile)
      (show! 'none 'score)
      (show! 'none 'highscore)
      ((base-layer 'add-drawable) score-text-tile)
      ((base-layer 'add-drawable) highscore-text-tile)
      ((press-space-tile 'set-y!) press-space-y)
      (draw-score! (game 'score))
      (draw-highscore! (game 'highscore)))

    ;;
    ;; STARTUP
    ;;

    (define splash-screen-tile (make-bitmap-tile "images/SplashScreen.png"))
    (define press-space-tile (make-bitmap-tile "images/Press-space.png"))

    (define (show-splash!)
      ((base-layer 'add-drawable) splash-screen-tile)
      ((press-space-tile 'set-y!) press-space-splash-y))

    (define (press-space show?)
      (if show?
          ((game-objects-layer 'remove-drawable) press-space-tile)
          ((game-objects-layer 'add-drawable) press-space-tile)))

    ;;
    ;; PUBLIC PROCEDURES
    ;;

    (define (initialize! game)
      (let ((level-object (game 'level)))
        (clear-tiles!)
        (base-layer 'empty)
        (game-objects-layer 'empty)
        (show! 'none 'score)
        (show! 'none 'highscore)
        (show! lives-sequence 'lives)
      
        ((base-layer 'add-drawable) score-text-tile)
        ((base-layer 'add-drawable) highscore-text-tile)
        ((base-layer 'add-drawable) lives-text-tile)

        ((game-objects-layer 'add-drawable) ant-sequence)

        (if (= (game 'current) first-level)
            (show! sky-tile 'sky))

        (map (lambda (kind) (draw! level-object kind)) '(wall
                                                         egg
                                                         key
                                                         bomb
                                                         door
                                                         weak-wall
                                                         shield-shroom
                                                         food))
        (draw-score! (game 'score))
        (draw-highscore! (game 'highscore))))

    (define (update! game)
      (let ((level-object (game 'level)))
        
        (draw-ant! level-object)
        (draw! level-object 'scorpion)

        (map (lambda (kind) (check-for-remove level-object kind)) '(egg
                                                                    shield-schroom
                                                                    food
                                                                    key
                                                                    bomb
                                                                    door
                                                                    weak-wall))
        (draw-lives! game)))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'show-splash!) (apply show-splash! parameters))
        ((eq? message 'press-space) (apply press-space parameters))
        ((eq? message 'initialize!) (apply initialize! parameters))
        ((eq? message 'update!) (apply update! parameters))
        ((eq? message 'game-over!) (apply game-over! parameters))
        ((eq? message 'game-win!) (apply game-win! parameters))
        ((eq? message 'update-score!) (apply draw-score! parameters))
        ((eq? message 'update-highscore!) (apply draw-highscore! parameters))
        ((eq? message 'speed-up) (apply speed-up parameters))
        ((eq? message 'shield) (apply shield parameters))
        (else (error "[ERROR in VisualADT DISPATCH] Wrong message!"))))

    dispatch))