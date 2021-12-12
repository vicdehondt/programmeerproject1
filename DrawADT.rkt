


(define grid-cell 24)

(define (make-draw)
  (let ((window (make-window 456 360 "Fire Ant")))

    ;;
    ;; Layer initialization
    ;;
    
    (define base-layer (window 'make-layer))

    (define egg-layer (window 'make-layer))

    (define moving-objects-layer (window 'make-layer))
    
    ;; Ant tiles

    ;(define ant (make-visual ((game-object 'level) 'ant) moving-objects-layer))
    (define ant-right-tile (make-bitmap-tile "images/FireAnt-Right.png" "images/FireAnt-Right-mask.png"))
    (define ant-left-tile (make-tile 24 24 "images/FireAnt-Left.png"))
    (define ant-left-right (make-tile-sequence '(ant-right-tile ant-left-tile)))
    ((moving-objects-layer 'add-drawable) ant-right-tile)


    ;;
    ;; Walls
    ;;

    (define wall-tiles '())
    (define egg-tiles '())
    (define scorpion-tiles '())

    (define (draw-wall! wall-object)
      (let ((tile (object-piece wall-object 'wall)))
        (draw-object! wall-object tile)))

    (define (draw-egg! egg-object)
      (let ((tile (object-piece egg-object 'egg)))
        (draw-object! egg-object tile)))
    
    (define (draw-scorpion! moving-object)
      (let ((tile (object-piece moving-object 'scorpion)))
        (draw-object! moving-object tile)))

    (define (which-tiles-list kind)
      (cond
        ((eq? kind 'wall) wall-tiles)
        ((eq? kind 'egg) egg-tiles)
        ((eq? kind 'scorpion) scorpion-tiles)))

    (define (which-tile kind)
      (cond
        ((eq? kind 'wall) (make-tile 24 24 "images/Wall.png"))
        ((eq? kind 'egg) (make-bitmap-tile "images/Egg.png" "images/Egg-mask.png"))
        ((eq? kind 'scorpion) (make-bitmap-tile "images/Scorpion.png" "images/Scorpion-mask.png"))))

    (define (which-layer kind)
      (cond
        ((eq? kind 'wall) base-layer)
        ((eq? kind 'egg) egg-layer)
        ((eq? kind 'scorpion) moving-objects-layer)))

    (define (object-piece object kind)
      (let* ((tiles (which-tiles-list kind))
             (result (assoc object tiles)))
        (if result
            (cdr result)
            (add-object-piece! object kind))))

    (define (add-object-piece! object kind)
      (let ((tiles (which-tiles-list kind))
            (layer (which-layer kind))
            (new-tile (which-tile kind)))
        (set! tiles (cons (cons object new-tile) tiles))
        ((layer 'add-drawable) new-tile)
        new-tile))

    ;;
    ;; Start procedure
    ;;
    
    (define (start! update-function key-function game-object)
      ((window 'set-background!) "black")
      ((window 'set-update-callback!) update-function)
      ((window 'set-key-callback!) key-function)
      (((game-object 'level) 'for-each-object) draw-wall! ((game-object 'level) 'walls))
      (((game-object 'level) 'for-each-object) draw-egg! ((game-object 'level) 'eggs))
      (((game-object 'level) 'for-each-object) draw-scorpion! ((game-object 'level) 'scorpions)))

    ;;
    ;; Update procedure
    ;;
    
    (define (update! game-object)
      (update-level! (game-object 'level)))

    (define (update-level! level-object)
      (draw-object! (level-object 'ant) ant-right-tile)
      ((level 'for-each-object) (lambda (scorpion) (draw-object! (car scorpion) (cdr scorpion))) scorpion-tiles))

    (define (draw-object! obj tile)
      (let* ((position (obj 'position))
             (new-x (* (position 'x) grid-cell))
             (new-y (* (position 'y) grid-cell)))
        ((tile 'set-x!) new-x)
        ((tile 'set-y!) new-y)))

    
    (define (dispatch msg)
      (cond ((eq? msg 'start!) start!)
            ((eq? msg 'update!) update!)))
    dispatch))