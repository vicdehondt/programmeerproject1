(#%require "Graphics.rkt")


(define grid-cell 24)

(define (make-draw)
  (let ((window (make-window 1920 1080 "Fire Ant")))

    ;;
    ;; Layer initialisatie
    ;;
    
    (define base-layer (window 'make-layer))

    (define moving-objects-layer (window 'make-layer))

    ;;
    ;; Ant tile
    ;;

    (define ant-right-tile (make-tile 24 24 "images/FireAnt-Right.png"))
    (define ant-left-tile (make-tile 24 24 "images/FireAnt-Left.png"))
    (define ant-left-right (make-tile-sequence '(ant-right-tile ant-left-tile)))
    ((moving-objects-layer 'add-drawable) ant-right-tile)


    ;;
    ;; Walls
    ;;
    
    
    ;;
    ;; Start procedure
    ;;
    
    (define (start! update-function key-function game-object)
      ((window 'set-background!) "black")
      ((window 'set-update-callback!) update-function)
      ((window 'set-key-callback!) key-function)
      (((game-object 'level) 'for-each-object) draw-wall! ((game-object 'level) 'walls)))

    
    (define (update! game-object)
      (update-level! (game-object 'level)))

    (define (update-level! level-object)
      (draw-object! (level-object 'ant) ant-right-tile))

    (define (draw-object! obj tile-to-right)
      (let* ((position (obj 'position))
             (new-x (* (position 'x) grid-cell))
             (new-y (* (position 'y) grid-cell)))
        ((tile-to-right 'set-x!) new-x)
        ((tile-to-right 'set-y!) new-y)))
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