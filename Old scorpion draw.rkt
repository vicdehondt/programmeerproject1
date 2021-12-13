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
        (display result);; Hier ergens zit de fout
        (if result
            (cdr result)
            (add-object-piece! object))))
#|
    (define (add-object-piece! object kind)
        (let ((tiles (which-tiles-list kind))
              (new-tile (which-tile kind)))
          (set! tiles (cons (cons object new-tile) tiles))
          (((which-layer kind) 'add-drawable) new-tile)
          new-tile))
|#
;#|
    (define (add-object-piece! object)
      (define (add-wall-piece! object)
        (let ((new-tile (which-tile 'wall)))
          (set! wall-tiles (cons (cons object new-tile) wall-tiles))
          ((base-layer 'add-drawable) new-tile)
          new-tile))
      
      (define (add-egg-piece! object)
        (let ((new-tile (which-tile 'egg)))
          (set! egg-tiles (cons (cons object new-tile) egg-tiles))
          ((egg-layer 'add-drawable) new-tile)
          new-tile))
      
      (define (add-scorpion-piece! object)
        (let ((new-tile (which-tile 'scorpion)))
          (set! scorpion-tiles (cons (cons object new-tile) scorpion-tiles))
          ((moving-objects-layer 'add-drawable) new-tile)
          new-tile))

      (cond
        ((eq? (object 'kind) 'wall) add-wall-piece!)
        ((eq? (object 'kind) 'egg) add-egg-piece!)
        ((eq? (object 'kind) 'scorpion) add-scorpion-piece!)))
;|#