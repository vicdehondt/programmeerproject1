(define (make-visual window)
  (let ((ant-sequence (make-tile-sequence (list (make-bitmap-tile "images/48px/FireAnt-Up.png" "images/48px/FireAnt-Up-mask.png")
                                                (make-bitmap-tile "images/48px/FireAnt-Right.png" "images/48px/FireAnt-Right-mask.png")
                                                (make-bitmap-tile "images/48px/FireAnt-Down.png" "images/48px/FireAnt-Down-mask.png")
                                                (make-bitmap-tile "images/48px/FireAnt-Left.png" "images/48px/FireAnt-Left-mask.png"))))
        (highscore-tile (make-tile 300 96))
        (score-tile (make-tile 250 96))
        (lives-tile (make-tile 200 96))
        (wall-tiles '())
        (egg-tiles '())
        (key-tiles '())
        (door-tiles '())
        (scorpion-tiles '()))
    

    
    
    ;;
    ;; LAYER INITIALIZATION
    ;;
    
    (define base-layer (window 'make-layer))
    (define egg-layer (window 'make-layer))
    (define moving-objects-layer (window 'make-layer))

    ;;
    ;; INFORMATION VISUALISATION
    ;;

    ((egg-layer 'add-drawable) highscore-tile)
    ((highscore-tile 'set-x!) 410)
    ((highscore-tile 'set-y!) 624)

    (define (highscore-text highscore)
      (string-append "Highscore: " (number->string highscore)))

    ((egg-layer 'add-drawable) score-tile)
    ((score-tile 'set-x!) 0)
    ((score-tile 'set-y!) 624)

    (define (score-text score)
      (string-append "Score: " (number->string score)))

    ((egg-layer 'add-drawable) lives-tile)
    ((lives-tile 'set-x!) 205)
    ((lives-tile 'set-y!) 624)

    (define (lives-text lives)
      (string-append "Lives: " (number->string lives)))

    ;;
    ;; DRAW PROCEDURES
    ;;

    ; Show the highscore on screen
    (define (draw-highscore! game)
      (let ((highscore (game 'highscore)))
        (highscore-tile 'clear)
        ((highscore-tile 'draw-text) (highscore-text highscore) 18 80 40 "white")))

    ; Show the score on screen
    (define (draw-score! game)
      (let ((score (game 'score)))
        (score-tile 'clear)
        ((score-tile 'draw-text) (score-text score) 18 80 40 "white")))

    ; Show lives remaining on screen
    (define (draw-lives! game)
      (let ((lives (game 'lives)))
        (lives-tile 'clear)
        ((lives-tile 'draw-text) (lives-text lives) 18 80 40 "white")))

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

    (define (draw-walls! level-object)
      (for-each-object (lambda (wall) (draw-stationary-piece! wall)) (level-object 'walls)))

    (define (draw-eggs! level-object)
      (for-each-object (lambda (egg) (draw-stationary-piece! egg)) (level-object 'eggs)))

    (define (draw-keys! level-object)
      (for-each-object (lambda (key) (draw-stationary-piece! key)) (level-object 'keys)))

    (define (draw-doors! level-object)
      (for-each-object (lambda (door) (draw-stationary-piece! door)) (level-object 'doors)))

    (define (draw-scorpions! level-object)
      (for-each-object (lambda (scorpion) (draw-scorpion-piece! scorpion)) (level-object 'scorpions)))

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
      (cond
        ((eq? kind 'scorpion) ((moving-objects-layer 'add-drawable) tile))
        ((eq? kind 'egg) ((egg-layer 'add-drawable) tile))
        ((eq? kind 'key) ((egg-layer 'add-drawable) tile))
        ((eq? kind 'door) ((egg-layer 'add-drawable) tile))
        ((eq? kind 'wall) ((base-layer 'add-drawable) tile))
        (else (error "[ERROR in VisualADT/show!] Wrong kind: ") (display kind))))

    
    ;;
    ;; EGG REMOVAL
    ;;

    ;; Finds the right egg to remove
    (define (which-egg-to-remove level-object)
      (let find ((current (car egg-tiles))
                 (remaining (cdr egg-tiles)))
        (if (not (level-object 'member? (car current) 'eggs))
            (begin
              ((egg-layer 'remove-drawable) (cdr current))
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
      (if (> (length egg-tiles) (level-object 'length? 'eggs))
          (set! egg-tiles (remove-from-list (which-egg-to-remove level-object) egg-tiles))))

    ;;
    ;; GAME OVER
    ;;

    (define game-over-tile (make-bitmap-tile "images/GameOver.png"))
    
    (define (game-over!)
      
      (base-layer 'empty)
      
      (egg-layer 'empty)
      
      (moving-objects-layer 'empty)
      
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
          ((moving-objects-layer 'remove-drawable) press-space-tile)
          ((moving-objects-layer 'add-drawable) press-space-tile)))

    ;;
    ;; PUBLIC PROCEDURES
    ;;
    
    (define (start! level-object)
      
      ((base-layer 'remove-drawable) splash-screen-tile)
      ((moving-objects-layer 'remove-drawable) press-space-tile)
      
      ;; Show ant on screen
      ((moving-objects-layer 'add-drawable) ant-sequence)
      
      (draw-walls! level-object))
    
    (define (update! game)
      (let ((level-object (game 'level)))
        
        (draw-ant! level-object)
        
        (draw-scorpions! level-object)
        
        (draw-eggs! level-object)

        (draw-keys! level-object)

        (draw-doors! level-object)
        
        (check-for-egg-remove level-object)

        (draw-highscore! game)
        
        (draw-score! game)
        
        (draw-lives! game)))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'start!) (apply start! parameters))
        ((eq? message 'update!) (apply update! parameters))
        ((eq? message 'game-over!) (apply game-over! parameters))
        ((eq? message 'show-splash!) (apply show-splash! parameters))
        ((eq? message 'press-space) (apply press-space parameters))
        (else (error "[ERROR in VisualADT DISPATCH] Wrong message: ") (display message))))

    dispatch))