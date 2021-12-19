(define (make-visual window)
  (let ((ant-sequence (make-tile-sequence (list (make-bitmap-tile "images/FireAnt-Right.png" "images/FireAnt-Right-mask.png")
                                                (make-bitmap-tile "images/FireAnt-Left.png" "images/FireAnt-Left-mask.png"))))
        (wall-tiles '())
        (egg-tiles '())
        (scorpion-tiles '()))
    

    ;;
    ;; Layer initialization
    ;;
    (define base-layer (window 'make-layer))
    (define egg-layer (window 'make-layer))
    (define moving-objects-layer (window 'make-layer))


    ;;
    ;; DRAW PROCEDURES
    ;;

    ;; Show ant on screen
    ((moving-objects-layer 'add-drawable) ant-sequence)

    ;; Lets the tile look to the direction it's going
    (define (set-sequence! object sequence)
      (let ((current-orientation (object 'orientation)))
        (cond
          ((and (eq? current-orientation 'right) (eq? (object 'old-orientation) 'left)) ((object 'old-orientation!) current-orientation)
                                                                                        (sequence 'set-next!))
          ((and (eq? current-orientation 'left) (eq? (object 'old-orientation) 'right)) ((object 'old-orientation!) current-orientation)
                                                                                        (sequence 'set-previous!)))))

    ;; Draws the ant
    (define (draw-ant! level-object)
      (let* ((tile ant-sequence)
             (ant (level-object 'ant)))
        (set-sequence! ant tile)
        (draw-object! ant tile)))

    ;; Draws every wall in the walls-list from the level
    (define (draw-walls! level-object)
      (for-each-object (lambda (wall) (draw-stationary-piece! wall level-object)) (level-object 'walls)))

    ;; Draws every egg in the eggs-list from the level
    (define (draw-eggs! level-object) ;; Elk egg-object uit de lijst eggs van het level moet worden getekend
      (for-each-object (lambda (egg) (draw-stationary-piece! egg level-object)) (level-object 'eggs)))

    ;; Draws every scorpion in the scorpion-list from the level
    (define (draw-scorpions! level-object)
      (for-each-object (lambda (scorpion) (draw-scorpion-piece! scorpion)) (level-object 'scorpions)))

    ;; Draws every scorpion
    (define (draw-scorpion-piece! scorpion-object)
      (let ((tile (get-object-piece scorpion-object)))
        (set-sequence! scorpion-object tile)
        (draw-object! scorpion-object tile)))

    ;; Draws every object that is not moving
    (define (draw-stationary-piece! object level-object)
      (let ((tile (get-object-piece object)))
        (draw-object! object tile)))

    ;; Looks for the right tile-list based on the kind of the object
    (define (which-tiles-list kind)
      (cond
        ((eq? kind 'wall) wall-tiles)
        ((eq? kind 'egg) egg-tiles)
        ((eq? kind 'scorpion) scorpion-tiles)))

    ;; Looks for the right tile bound to the given object
    (define (get-object-piece object)
      (let* ((tile-list (which-tiles-list (object 'kind)))
             (result (assoc object tile-list)))
        (if result
            (cdr result)
            (add-object-piece! object))))

    ;; Adds a new object bound to its tile to the tile-list
    (define (add-object-piece! object)
      (let* ((kind (object 'kind))
             (new-scorpion-sequence (make-tile-sequence (list (make-bitmap-tile "images/Scorpion.png" "images/Scorpion-mask.png")
                                                              (make-bitmap-tile "images/Scorpion2.png" "images/Scorpion2-mask.png"))))
             (new-egg-tile (make-bitmap-tile "images/Egg.png" "images/Egg-mask.png"))
             (new-wall-tile (make-tile 24 24 "images/Wall.png")))
        
        (define (add-piece new-tile)
          (add-object-tile! object new-tile)
          (show! new-tile kind)
          new-tile)
        
        (cond
          ((eq? kind 'scorpion) (add-piece new-scorpion-sequence))
          ((eq? kind 'egg) (add-piece new-egg-tile))
          ((eq? kind 'wall) (add-piece new-wall-tile))
          (else (error "Wrong kind given!")))))

    ;; Draws a given object in the window
    (define (draw-object! obj tile)
      (let* ((position (obj 'position))
             (new-x (* (position 'x) grid-cell))
             (new-y (* (position 'y) grid-cell)))
        ((tile 'set-x!) new-x)
        ((tile 'set-y!) new-y)))

    ;; Help procedure for adding object and its tile
    (define (add-object-tile! object tile )
      (let ((kind (object 'kind)))
        (cond
          ((eq? kind 'scorpion) (set! scorpion-tiles (cons (cons object tile) scorpion-tiles)))
          ((eq? kind 'egg) (set! egg-tiles (cons (cons object tile) egg-tiles)))
          ((eq? kind 'wall) (set! wall-tiles (cons (cons object tile) wall-tiles)))
          (else (error "Wrong kind given!")))))

    ;; Help procedure to show the tile in the window (add-drawable)
    (define (show! tile kind)
      (cond
        ((eq? kind 'scorpion) ((moving-objects-layer 'add-drawable) tile))
        ((eq? kind 'egg) ((egg-layer 'add-drawable) tile))
        ((eq? kind 'wall) ((base-layer 'add-drawable) tile))
        (else (error "Wrong kind given!"))))

    
    ;;
    ;; EGG REMOVAL
    ;;

    ;; Finds the right egg to remove
    (define (which-egg-to-remove level-object)
      (let find ((current (car egg-tiles))
                 (remaining (cdr egg-tiles)))
        (if (not (member (car current) (level-object 'eggs)))
            (begin
              ((egg-layer 'remove-drawable) (cdr current))
              current)
            (which-egg-object (car remaining) (cdr remaining)))))

    ;; Checks if an egg is removed
    (define (check-for-egg-remove level-object)
      (if (> (length egg-tiles) (length (level-object 'eggs)))
          (set! egg-tiles (remove-from-list (which-egg-to-remove level-object) egg-tiles))))


    ;;
    ;; PUBLIC PROCEDURES
    ;;

    ;; Executes once
    (define (start! level-object)
      (draw-walls! level-object))
    
    ;; Update loop
    (define (update! level-object)
      (draw-ant! level-object)
      (draw-scorpions! level-object)
      (draw-eggs! level-object)
      (check-for-egg-remove level-object))
    
  
    (define (dispatch m)
      (cond
        ((eq? m 'start!) start!)
        ((eq? m 'update!) update!)))
    dispatch))