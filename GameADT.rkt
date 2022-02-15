(define (make-game level)
  (let ((draw (make-draw)))

    (define (start!)
      (draw 'start! game-loop key-callback level))

    ;; What happens every tick
    (define (game-loop delta-time)
      (draw 'update! level)
      (level 'move-scorpion! delta-time))

    ;; What to do when a key is pressed
    (define (key-callback status key)
      (if (eq? status 'pressed)
            (level 'move-ant! key)))

    (lambda (message . parameters)
      (cond
        ((eq? message 'start!) (start!))
        ((eq? message 'level) level)
        (else  (error "[ERROR in GameADT DISPATCH] Wrong message: ") (display message))))))
