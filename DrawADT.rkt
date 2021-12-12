


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

    (define (draw-wall! wall-object)
      (let ((tile (object-piece wall-object 'wall)))
        (draw-object! wall-object tile)))

    (define (draw-egg! egg-object)
      (let ((tile (object-piece egg-object 'egg)))
        (draw-object! egg-object tile)))

    (define (which-tiles-list kind)
      (cond
        ((eq? kind 'wall) wall-tiles)
        ((eq? kind 'egg) egg-tiles)))

    (define (which-tile kind)
      (cond
        ((eq? kind 'wall) (make-tile 24 24 "images/Wall.png"))
        ((eq? kind 'egg) (make-bitmap-tile "images/Egg.png" "images/Egg-mask.png"))))

    (define (object-piece object kind)
      (let* ((tiles (which-tiles-list kind))
             (result (assoc object tiles)))
        (if result
            (cdr result)
            (add-object-piece! object kind))))

    (define (add-object-piece! object kind)
      (let ((tiles (which-tiles-list kind))
            (new-tile (which-tile kind)))
        (set! tiles (cons (cons object new-tile) tiles))
        ((base-layer 'add-drawable) new-tile)
        new-tile))

    ;;
    ;; Start procedure
    ;;
    
    (define (start! update-function key-function game-object)
      ((window 'set-background!) "black")
      ((window 'set-update-callback!) update-function)
      ((window 'set-key-callback!) key-function)
      (((game-object 'level) 'for-each-object) draw-wall! ((game-object 'level) 'walls))
      (((game-object 'level) 'for-each-object) draw-egg! ((game-object 'level) 'eggs)))

    ;;
    ;; Update procedure
    ;;
    
    (define (update! game-object)
      (update-level! (game-object 'level)))

    (define (update-level! level-object)
      ;(ant 'update!)
      (draw-object! (level-object 'ant) ant-right-tile))

    (define (draw-object! obj tile)
      (let* ((position (obj 'position))
             (new-x (* (position 'x) grid-cell))
             (new-y (* (position 'y) grid-cell)))
        ((tile 'set-x!) new-x)
        ((tile 'set-y!) new-y)))
    
    #|
    (define (draw-orientation! object tile-to-left tile-to-right)
      (let ((current (object 'orientation)))
        (cond
          ((eq? current 'left) (ant-left-right))
          ((eq? current 'right) ((moving-objects-layer 'add-drawable) tile-to-right)
                                ((moving-objects-layer 'remove-drawable) tile-to-left)))))
    |#

    
    (define (dispatch msg)
      (cond ((eq? msg 'start!) start!)
            ((eq? msg 'update!) update!)))
    dispatch))