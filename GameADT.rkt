(define (make-game level)
  (let ((draw (make-draw)))

    (define (start!)
      ((draw 'start!) game-loop key-callback dispatch))

    (define (game-loop delta-time)
      ((draw 'update!) dispatch)
      ;(level 'check-eggs!)
      ((level 'move-scorpion!) delta-time))

    (define (key-callback status key)
      (if (eq? status 'pressed)
          (begin
            ((level 'move-ant!) key))))
  
    (define (dispatch m)
      (cond
        ((eq? m 'start!) (start!))
        ((eq? m 'level) level)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))
