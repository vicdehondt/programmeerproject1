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
    ;(define (initialize-ant level-object)
    ;  ((ant-right-tile 'set-x!) (((level-object 'ant) 'position) 'x))
    ;  ((ant-right-tile 'set-y!) (((level-object 'ant) 'position) 'y)))
    ((moving-objects-layer 'add-drawable) ant-right-tile)

    ;;
    ;; Start procedure
    ;;
    
    (define (start! key-function)
      ((window 'set-background!) "black")
      ;((window 'set-update-callback!) update-function)
      ((window 'set-key-callback!) key-function))

    (define (update! game-object)
      (update-level! (game-object 'level)))

    (define (draw-object! obj tile)
      (let* ((position (obj 'position))
            (new-x (* (position 'x) grid-cell))
            (new-y (* (position 'y) grid-cell)))
        ((tile 'set-x!) new-x)
        ((tile 'set-y!) new-y)))

    (define (update-level! level-object)
      (draw-object! (level-object 'ant) ant-right-tile))

    
    (define (dispatch msg)
      (cond ((eq? msg 'start!) start!)
            ((eq? msg 'update!) update!)))
    dispatch))