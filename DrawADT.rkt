(define (make-draw)
  (let* ((window (make-window width height "Fire Ant" 200))
         (visual (make-visual window)))
    
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
      ((window 'set-background!) "black")
      ((window 'set-update-callback!) update-function)
      (visual 'start! game))

    (define (initialize! game)
      (visual 'initialize! game))

    (define (update-score! score)
      (visual 'update-score! score))

    (define (update-highscore! highscore)
      (visual 'update-highscore! highscore))

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
            ((eq? message 'game-over!) (visual 'game-over!))
            ((eq? message 'wall-tiles) (visual 'wall-tiles))
            (else  (error "[ERROR in DrawADT DISPATCH] Wrong message!"))))

    dispatch))