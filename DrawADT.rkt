(define (make-draw)
  (let* ((window (make-window width height "Fire Ant" 200))
         (visual (make-visual window))
         (black? #t))
    
    ;;
    ;; START PROCEDURE
    ;;

    (define (show-splash! key-function update-function)
      (visual 'show-splash!)
      ((window 'set-key-callback!) key-function)
      ((window 'set-update-callback!) update-function))

    (define (press-space shown?)
      (visual 'press-space shown?))
    
    (define (start! update-function game)
      (set-black!)
      ((window 'set-update-callback!) update-function)
      (visual 'start! game))

    (define (initialize! game)
      (visual 'initialize! game))

    (define (continue! update-function)
      ;(set-black!)
      ((window 'set-update-callback!) update-function))

    (define (update-score! score)
      (visual 'update-score! score))

    (define (update-highscore! highscore)
      (visual 'update-highscore! highscore))

    (define (game-over! update-function)
      (visual 'game-over!)
      ((window 'set-update-callback!) update-function))
    
    (define (game-win! update-function)
      (visual 'game-win!)
      ((window 'set-update-callback!) update-function))

    (define (bomb-animation! update-function)
      ((window 'set-update-callback!) update-function))

    (define (set-opposite-background!)
      (if black?
          (set-white!)
          (set-black!)))

    (define (set-black!)
      ((window 'set-background!) "black")
      (set! black? #t))

    (define (set-white!)
      ((window 'set-background!) "white")
      (set! black? #f))

    ;;
    ;; UPDATE PROCEDURE
    ;;
    
    (define (update! game)
       (visual 'update! game))

    (define (dispatch message . parameters)
      (cond ((eq? message 'start!) (apply start! parameters))
            ((eq? message 'update!) (apply update! parameters))
            ((eq? message 'show-splash!) (apply show-splash! parameters))
            ((eq? message 'press-space) (apply press-space parameters))
            ((eq? message 'initialize!) (apply initialize! parameters))
            ((eq? message 'update-score!) (apply update-score! parameters))
            ((eq? message 'update-highscore!) (apply update-highscore! parameters))
            ((eq? message 'game-over!) (apply game-over! parameters))
            ((eq? message 'game-win!) (apply game-win! parameters))
            ((eq? message 'bomb-animation!) (apply bomb-animation! parameters))
            ((eq? message 'speed-up) (visual 'speed-up parameters))
            ((eq? message 'continue!) (apply bomb-animation! parameters))
            ((eq? message 'set-black!) (apply set-black! parameters))
            ((eq? message 'set-opposite-background!) (apply set-opposite-background! parameters))
            (else  (error "[ERROR in DrawADT DISPATCH] Wrong message!"))))

    dispatch))