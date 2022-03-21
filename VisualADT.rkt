(define (make-visual window)
  (let ((ant-sequence (make-tile-sequence (list (make-bitmap-tile "images/48px/FireAnt-Up.png" "images/48px/FireAnt-Up-mask.png")
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
        (highscore-text-tile (make-bitmap-tile "images/Highscore/Highscore.png" "images/Highscore/Highscore-mask.png"))
        (lives-text-tile (make-bitmap-tile "images/Lives/Lives.png" "images/Lives/Lives-mask.png"))
        (score (make-vector 8 0))
        (highscore (make-vector 8 0))
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
        (door-tiles '())
        (scorpion-tiles '()))
    

    
    
    ;;
    ;; LAYER INITIALIZATION
    ;;
    
    (define base-layer (window 'make-layer))
    (define game-objects-layer (window 'make-layer))

    ;;
    ;; INFORMATION VISUALISATION
    ;;

    ((score-text-tile 'set-x!) 168)
    ((score-text-tile 'set-y!) 624)

    ((highscore-text-tile 'set-x!) 0)
    ((highscore-text-tile 'set-y!) 656)

    ((lives-text-tile 'set-x!) 648)
    ((lives-text-tile 'set-y!) 624)

    ((lives-sequence 'set-x!) 884)
    ((lives-sequence 'set-y!) 624)

    ;;
    ;; DRAW PROCEDURES
    ;;
    
    ; Show the highscore on screen
    (define (draw-highscore! game)
      (let loop ((game-highscore (game 'highscore))
                 (count 0))
        (if (<= count (- score-size 1))
            (begin
              (((vector-ref highscore count) 'set-current!) (vector-ref game-highscore count))
              (loop game-highscore (+ count 1))))))

    (define (draw-score! game)
      (let loop ((game-score (game 'score))
                 (count 0))
        (if (<= count (- score-size 1))
            (begin
              (((vector-ref score count) 'set-current!) (vector-ref game-score count))
              (loop game-score (+ count 1))))))

    ; Show lives remaining on screen
    (define (draw-lives! game)
      (let ((lives (game 'lives)))
        ((lives-sequence 'set-current!) lives)))

    ;; Lets the tile look to the direction it's going
    (define (set-sequence! object sequence)
      (let ((current-orientation (object 'orientation)))
        (cond
          ((and (eq? current-orientation 'right) (eq? (object 'previous-orientation) 'left)) (object 'previous-orientation! current-orientation)
                                                                                             (sequence 'set-next!))
          ((and (eq? current-orientation 'left) (eq? (object 'previous-orientation) 'right)) (object 'previous-orientation! current-orientation)
                                                                                             (sequence 'set-previous!)))))

    (define (set-current-tile! orientation sequence)
      (cond
        ((eq? orientation 'up) ((sequence 'set-current!) 0))
        ((eq? orientation 'right) ((sequence 'set-current!) 1))
        ((eq? orientation 'down) ((sequence 'set-current!) 2))
        ((eq? orientation 'left) ((sequence 'set-current!) 3))
        (else (error "[ERROR in VisualADT/set-current-tile!] Wrong orientation: ") (display orientation))))

    (define (draw-ant! level-object)
      (let* ((ant (level-object 'ant)))
        (set-current-tile! (ant 'orientation) ant-sequence)
        (draw-object! ant ant-sequence)))

    (define (draw! level-object kind)
      (cond
        ((eq? kind 'wall) (for-each-object (lambda (wall) (draw-stationary-piece! wall)) (level-object 'walls)))
        ((eq? kind 'egg) (for-each-object (lambda (egg) (draw-stationary-piece! egg)) (level-object 'eggs)))
        ((eq? kind 'key) (for-each-object (lambda (key) (draw-stationary-piece! key)) (level-object 'keys)))
        ((eq? kind 'door) (for-each-object (lambda (door) (draw-stationary-piece! door)) (level-object 'doors)))
        ((eq? kind 'scorpion) (for-each-object (lambda (scorpion) (draw-scorpion-piece! scorpion)) (level-object 'scorpions)))))

    (define (draw-scorpion-piece! scorpion-object)
      (let ((sequence (get-object-piece scorpion-object))
            (orientation (scorpion-object 'orientation)))
        (set-current-tile! orientation sequence)
        (draw-object! scorpion-object sequence)))

    (define (draw-stationary-piece! object)
      (let ((tile (get-object-piece object)))
        (draw-object! object tile)))

    (define (which-tiles-list kind)
      (cond
        ((eq? kind 'wall) wall-tiles)
        ((eq? kind 'egg) egg-tiles)
        ((eq? kind 'key) key-tiles)
        ((eq? kind 'door) door-tiles)
        ((eq? kind 'scorpion) scorpion-tiles)))

    ;; Looks for the right tile bound to the given object
    (define (get-object-piece object)
      (let* ((tile-list (which-tiles-list (object 'kind)))
             (result (assoc object tile-list)))
        (if result
            (cdr result)
            (add-object-piece! object))))

    (define (add-object-piece! object)
      (let* ((kind (object 'kind))
             (new-scorpion-sequence (make-tile-sequence (list (make-bitmap-tile "images/48px/Scorpion-Up.png" "images/48px/Scorpion-Up-mask.png")
                                                              (make-bitmap-tile "images/48px/Scorpion-Right.png" "images/48px/Scorpion-Right-mask.png")
                                                              (make-bitmap-tile "images/48px/Scorpion-Down.png" "images/48px/Scorpion-Down-mask.png")
                                                              (make-bitmap-tile "images/48px/Scorpion-Left.png" "images/48px/Scorpion-Left-mask.png"))))
             (new-egg-tile (make-bitmap-tile "images/48px/Egg.png" "images/48px/Egg-mask.png"))
             (new-key-tile (make-bitmap-tile "images/48px/Key.png" "images/48px/Key-mask.png"))
             (new-door-tile (make-tile 48 48 "images/48px/Door.png"))
             (new-wall-tile (make-tile 48 48 "images/48px/Wall.png")))
        
        (define (add-piece new-tile)
          (add-object-tile! object new-tile)
          (show! new-tile kind)
          new-tile)
        
        (cond
          ((eq? kind 'scorpion) (add-piece new-scorpion-sequence))
          ((eq? kind 'egg) (add-piece new-egg-tile))
          ((eq? kind 'key) (add-piece new-key-tile))
          ((eq? kind 'door) (add-piece new-door-tile))
          ((eq? kind 'wall) (add-piece new-wall-tile))
          (else (error "[ERROR in VisualADT/add-object-piece!] Wrong kind: ") (display kind)))))

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
          ((eq? kind 'scorpion) (set! scorpion-tiles (cons (cons object tile) scorpion-tiles)))
          ((eq? kind 'egg) (set! egg-tiles (cons (cons object tile) egg-tiles)))
          ((eq? kind 'key) (set! key-tiles (cons (cons object tile) key-tiles)))
          ((eq? kind 'door) (set! door-tiles (cons (cons object tile) door-tiles)))
          ((eq? kind 'wall) (set! wall-tiles (cons (cons object tile) wall-tiles)))
          (else (error "[ERROR in VisualADT/add-object-tile!] Wrong kind: ") (display kind)))))

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
              (show-score! (+ count 1) (+ x 31) y vector kind))))
      (cond
        ((eq? kind 'scorpion) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'egg) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'key) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'door) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'wall) ((game-objects-layer 'add-drawable) tile))
        ((eq? kind 'score) (show-score! 0 372 624 score 'score))
        ((eq? kind 'highscore) (show-score! 0 372 656 highscore 'highscore))
        ((eq? kind 'lives) ((game-objects-layer 'add-drawable) tile))
        (else (error "[ERROR in VisualADT/show!] Wrong kind: ") (display kind))))

    
    ;;
    ;; REMOVING
    ;;

    (define (which-egg-to-remove level-object)
      (let find ((current (car egg-tiles))
                 (remaining (cdr egg-tiles)))
        (if (not (level-object 'member? (car current) 'egg))
            (begin
              ((game-objects-layer 'remove-drawable) (cdr current))
              current)
            (find (car remaining) (cdr remaining)))))

    (define (which-key-to-remove level-object)
      (let find ((current (car key-tiles))
                 (remaining (cdr key-tiles)))
        (if (not (level-object 'member? (car current) 'key))
            (begin
              ((game-objects-layer 'remove-drawable) (cdr current))
              current)
            (find (car remaining) (cdr remaining)))))

    (define (which-door-to-remove level-object)
      (let find ((current (car door-tiles))
                 (remaining (cdr door-tiles)))
        (if (not (level-object 'member? (car current) 'door))
            (begin
              ((game-objects-layer 'remove-drawable) (cdr current))
              current)
            (find (car remaining) (cdr remaining)))))
    
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

    ;; Checks if an egg is removed
    (define (check-for-egg-remove level-object)
      (if (> (length egg-tiles) (level-object 'length? 'egg))
          (set! egg-tiles (remove-from-list (which-egg-to-remove level-object) egg-tiles))))

    (define (check-for-key-remove level-object)
      (if (> (length key-tiles) (level-object 'length? 'key))
          (set! key-tiles (remove-from-list (which-key-to-remove level-object) key-tiles))))

    (define (check-for-door-remove level-object)
      (if (> (length door-tiles) (level-object 'length? 'door))
          (set! door-tiles (remove-from-list (which-door-to-remove level-object) door-tiles))))

    ;;
    ;; GAME OVER
    ;;

    (define game-over-tile (make-bitmap-tile "images/GameOver.png"))
    
    (define (game-over!)
      
      (base-layer 'empty)
      
      (game-objects-layer 'empty)
      
      ((base-layer 'add-drawable) game-over-tile))

    ;;
    ;; STARTUP
    ;;

    (define splash-screen-tile (make-bitmap-tile "images/SplashScreen.png"))
    (define press-space-tile (make-bitmap-tile "images/Press-space.png"))
    ((press-space-tile 'set-y!) 600)

    (define (show-splash!)
      ((base-layer 'add-drawable) splash-screen-tile))

    (define (press-space show?)
      (if show?
          ((game-objects-layer 'remove-drawable) press-space-tile)
          ((game-objects-layer 'add-drawable) press-space-tile)))

    ;;
    ;; PUBLIC PROCEDURES
    ;;
    
    (define (start! level-object)
      
      ((base-layer 'remove-drawable) splash-screen-tile)
      ((game-objects-layer 'remove-drawable) press-space-tile)
      
      ;; Show ant on screen
      ((game-objects-layer 'add-drawable) ant-sequence)
      ((base-layer 'add-drawable) score-text-tile)
      ((base-layer 'add-drawable) highscore-text-tile)
      ((base-layer 'add-drawable) lives-text-tile)
      (show! 'none 'score)
      (show! 'none 'highscore)
      (show! lives-sequence 'lives)

      (draw! level-object 'wall))
    
    (define (update! game)
      (let ((level-object (game 'level)))
        
        (draw-ant! level-object)

        (draw! level-object 'egg)
        (draw! level-object 'key)
        (draw! level-object 'door)
        (draw! level-object 'scorpion)

        (check-for-egg-remove level-object)
        (check-for-key-remove level-object)
        (check-for-door-remove level-object)

        (draw-highscore! game)
        
        (draw-score! game)
        
        (draw-lives! game)
        ))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'start!) (apply start! parameters))
        ((eq? message 'update!) (apply update! parameters))
        ((eq? message 'game-over!) (apply game-over! parameters))
        ((eq? message 'show-splash!) (apply show-splash! parameters))
        ((eq? message 'press-space) (apply press-space parameters))
        (else (error "[ERROR in VisualADT DISPATCH] Wrong message: ") (display message))))

    dispatch))