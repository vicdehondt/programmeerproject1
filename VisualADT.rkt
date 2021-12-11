(define (make-visual object layer)

  (define (tile object)
    (cond
      ((eq? (object 'kind) 'ant) ((moving-objects-layer 'add-drawable) (make-bitmap-tile "images/FireAnt-Right.png"
                                                                                         "images/FireAnt-Right-mask.png")))
      ((eq? (object 'kind) 'scorpion) (display "Not yet implemented"))
      ((eq? (object 'kind) 'egg) (make-bitmap-tile "images/Egg.png" "images/Egg-mask"))
      ((eq? (object 'kind) 'wall) (make-tile 24 24 "images/Wall.png"))))

  (define (update!)
      (let* ((position (object 'position))
             (new-x (* (position 'x) grid-cell))
             (new-y (* (position 'y) grid-cell)))
        ((tile 'set-x!) new-x)
        ((tile 'set-y!) new-y)))
  
  (define (dispatch m)
    (cond
      ((eq? m 'update!) (display "test"))))
  dispatch)