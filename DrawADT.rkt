
(define grid-cell 24)

(define (make-draw)
  (let ((window (make-window 456 360 "Fire Ant"))
        (ant-old-orientation 'right)
        (scorpion-old-orientation 'right))

    ;;
    ;; Layer initialization
    ;;
    
    (define base-layer (window 'make-layer))
    (define egg-layer (window 'make-layer))
    (define moving-objects-layer (window 'make-layer))
    
    ;; Ant Tile-sequence
    (define ant-sequence (make-tile-sequence (list (make-bitmap-tile "images/FireAnt-Right.png" "images/FireAnt-Right-mask.png")
                                                   (make-bitmap-tile "images/FireAnt-Left.png" "images/FireAnt-Left-mask.png")
                                                   ;(make-bitmap-tile "images/FireAnt-Up.png" "images/FireAnt-Up-mask.png")
                                                   ;(make-bitmap-tile "images/FireAnt-Down.png" "images/FireAnt-Down-mask.png")
                                                   )))

    ((moving-objects-layer 'add-drawable) ant-sequence)

    (define scorpion-sequence (make-tile-sequence (list (make-bitmap-tile "images/Scorpion.png" "images/Scorpion-mask.png")
                                                        (make-bitmap-tile "images/Scorpion2.png" "images/Scorpion2-mask.png"))))

    


    ;; Tile-lists
    (define wall-tiles '())
    (define egg-tiles '())
    (define scorpion-tiles '())

    ;;
    ;; Draw Procedures
    ;;

    
    (define (set-ant-sequence! current-orientation sequence)
      (cond
        ((and (eq? current-orientation 'right) (eq? ant-old-orientation 'left)) (set! ant-old-orientation current-orientation)
                                                                                (sequence 'set-next!))
        ((and (eq? current-orientation 'left) (eq? ant-old-orientation 'right)) (set! ant-old-orientation current-orientation)
                                                                                (sequence 'set-previous!))
        #|((and (eq? current-orientation 'down) (eq? ant-old-orientation 'up)) (set! ant-old-orientation current-orientation)
                                                                 (sequence 'set-next!))
        ((and (eq? current-orientation 'up) (eq? ant-old-orientation down)) (set! ant-old-orientation current-orientation)
                                                                (sequence 'set-previous!))|#))

    (define (set-scorpion-sequence! current sequence)
      (cond
        ((and (eq? current 'right) (eq? scorpion-old-orientation 'left)) (set! scorpion-old-orientation current)
                                                                         (sequence 'set-next!))
        ((and (eq? current 'left) (eq? scorpion-old-orientation 'right)) (set! scorpion-old-orientation current)
                                                                         (sequence 'set-previous!))))

    (define (draw-ant! level-object)
      (let* ((tile ant-sequence)
             (ant (level-object 'ant)))
        (set-ant-sequence! (ant 'orientation) tile)
        (draw-object! ant tile)))

    (define (which-tiles-list kind)
      (cond
        ((eq? kind 'wall) wall-tiles)
        ((eq? kind 'egg) egg-tiles)
        ((eq? kind 'scorpion) scorpion-tiles)))
    
    (define (draw-walls! level-object)
      (for-each-object (lambda (wall) (draw-wall-piece! wall (level-object 'walls))) (level-object 'walls)))

    (define (draw-eggs! level-object) ;; Elk egg-object uit de lijst eggs van het level moet worden getekend
      (for-each-object (lambda (egg) (draw-egg-piece! egg (level-object 'eggs))) (level-object 'eggs)))

    (define (draw-scorpions! level-object)
      (for-each-object (lambda (scorpion) (draw-scorpion-piece! scorpion (level-object 'scorpions))) (level-object 'scorpions)))

    (define (draw-wall-piece! wall-object level-list)
      (let ((tile (get-object-piece wall-object level-list)))
        (draw-object! wall-object tile)))

    (define (draw-egg-piece! egg-object level-list) ;; De tile die samenhoort met het object wordt gezocht en getekend
      (let ((tile (get-object-piece egg-object level-list)))
        (draw-object! egg-object tile)))
    
    (define (draw-scorpion-piece! scorpion-object level-list)
      (let ((tile (get-object-piece scorpion-object level-list)))
        (set-scorpion-sequence! (scorpion-object 'orientation) tile)
        (draw-object! scorpion-object tile)))

    (define (get-object-piece object level-list)
      (let* ((tile-list (which-tiles-list (object 'kind)))
             (result (assoc object tile-list)))
        (cond
          (result (cdr result))
          ((and (not result) (> (length level-list) (length tile-list))) (add-object-piece! object))
          )))
        #|(if result
            (cdr result)
            (add-object-piece! object))))|#

    ;(define (remove-object-piece! object))

    (define (add-object-piece! object)
      (define (add-wall-piece! wall-object)
        (let ((new-tile
               (make-tile 24 24 "images/Wall.png")))
          (set! wall-tiles (cons (cons wall-object new-tile) wall-tiles))
          ((base-layer 'add-drawable) new-tile)
          new-tile))
      
      (define (add-egg-piece! egg-object)
        (let ((new-tile
               (make-bitmap-tile "images/Egg.png" "images/Egg-mask.png")))
          (set! egg-tiles (cons (cons egg-object new-tile) egg-tiles))
          ((egg-layer 'add-drawable) new-tile)
          new-tile))
      
      (define (add-scorpion-piece! scorpion-object)
        (let ((new-tile scorpion-sequence))
          (set! scorpion-tiles (cons (cons scorpion-object new-tile) scorpion-tiles))
          ((moving-objects-layer 'add-drawable) new-tile)
          new-tile))

      (cond
        ((eq? (object 'kind) 'wall) (add-wall-piece! object))
        ((eq? (object 'kind) 'egg) (add-egg-piece! object))
        ((eq? (object 'kind) 'scorpion) (add-scorpion-piece! object))))

    (define (draw-object! obj tile)
      (let* ((position (obj 'position))
             (new-x (* (position 'x) grid-cell))
             (new-y (* (position 'y) grid-cell)))
        ((tile 'set-x!) new-x)
        ((tile 'set-y!) new-y)))
    
    ;;
    ;; Start procedure
    ;;
    
    (define (start! update-function key-function game-object)
      ((window 'set-background!) "black")
      ((window 'set-update-callback!) update-function)
      ((window 'set-key-callback!) key-function)
      (draw-walls! (game-object 'level)))

    ;;
    ;; Update procedure
    ;;
    
    (define (update! game-object)
      (update-level! (game-object 'level)))
    
    (define (update-level! level-object)
      (draw-ant! level-object)
      (draw-scorpions! level-object)
      (draw-eggs! level-object))

    (define (dispatch msg)
      (cond ((eq? msg 'start!) start!)
            ((eq? msg 'update!) update!)))
    dispatch))