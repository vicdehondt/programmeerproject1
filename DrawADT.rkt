(#%require "Graphics.rkt")


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

    (define (draw-wall! wall-object)
      (let ((tile (wall-piece wall-object)))
        (draw-object! wall-object tile)))

    (define (wall-piece wall-object)
      (let ((result (assoc wall-object wall-tiles)))
        (if result
            (cdr result)
            (add-wall-piece! wall-object))))

    (define (add-wall-piece! wall-object)
      (let ((new-tile
              (make-tile 24 24 "images/Wall.png")))
        (set! wall-tiles (cons (cons wall-object new-tile) wall-tiles))
        ((base-layer 'add-drawable) new-tile)
        new-tile))

    ;;
    ;; Eggs
    ;;

    (define egg-tiles '())

    (define (draw-egg! egg-object)
      (let ((tile (egg-piece egg-object)))
        (draw-object! egg-object tile)))

    (define (egg-piece egg-object)
      (let ((result (assoc egg-object egg-tiles)))
        (if result
            (cdr result)
            (add-egg-piece! egg-object))))

    (define (add-egg-piece! egg-object)
      (let ((new-tile
              (make-bitmap-tile "images/Egg.png" "images/Egg-mask")))
        (set! egg-tiles (cons (cons egg-object new-tile) egg-tiles))
        ((egg-layer 'add-drawable) new-tile)
        new-tile))
    
    ;;
    ;; Start procedure
    ;;
    
    (define (start! update-function key-function game-object)
      ((window 'set-background!) "black")
      ((window 'set-update-callback!) update-function)
      ((window 'set-key-callback!) key-function)
      (((game-object 'level) 'for-each-object) draw-wall! ((game-object 'level) 'walls)))

    ;;
    ;; Update procedure
    ;;
    
    (define (update! game-object)
      (update-level! (game-object 'level)))

    (define (update-level! level-object)
      ;(ant 'update!)
      (draw-object! (level-object 'ant) ant-right-tile)
      ((level-object 'for-each-object) draw-egg! (level-object 'eggs)))

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