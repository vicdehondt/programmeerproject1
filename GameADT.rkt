(define (make-game level)
  (let ((draw (make-draw))
        (ant-time 0))

    (define (start!)
      ((draw 'start!) key-callback))

    ;(define (game-loop delta-time)
    ;  ((draw 'update!) delta-time))

    (define (key-callback status key)
      (if (eq? status 'pressed)
          (begin
            ((level 'move!) key)
            ((draw 'update!) dispatch))))
  
    (define (dispatch m)
      (cond
        ((eq? m 'start!) (start!))
        ((eq? m 'level) level)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))
