(define (make-game level)
  (let ((draw (make-draw)))

    (define (start!)
      ((draw 'start!) game-loop key-callback dispatch))

    ;; What happens every tick
    (define (game-loop delta-time)
      ((draw 'update!) dispatch)
      ((level 'move-scorpion!) delta-time))

    ;; What to do when a key is pressed
    (define (key-callback status key)
      (if (eq? status 'pressed)
            ((level 'move-ant!) key)))
  
    (define (dispatch m)
      (cond
        ((eq? m 'start!) (start!))
        ((eq? m 'level) level)
        (else  (error "[ERROR in GameADT DISPATCH] Wrong message: ") (display m))))
    dispatch))
