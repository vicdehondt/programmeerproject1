(define (make-game level)
  (let ((draw (make-draw))
        (ant-time 0))

    (define (start!)
      ((draw 'start!) game-loop key-callback dispatch))

    (define (game-loop delta-time)
      ((draw 'update!) dispatch))

    (define (key-callback status key)
      (if (eq? status 'pressed)
          (begin
            ((level 'move!) key))))
#|
    (define (move! key)
      (cond
        ((eq? key 'right)
         (((level 'ant) 'move-left) 1))
        ((eq? key 'left)
         (((level 'ant) 'move-left) 1))
        ((eq? key 'up)
         (((level 'ant) 'move-up) 1))
        ((eq? key 'down)
         (((level 'ant) 'move-down) 1))))

    (define (set-orientation))

|#
  
    (define (dispatch m)
      (cond
        ((eq? m 'start!) (start!))
        ((eq? m 'level) level)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))
