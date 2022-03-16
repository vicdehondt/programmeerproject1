(define (make-draw)
  (let* ((window (make-window width height "Fire Ant"))
         (visual (make-visual window)))
    
    ;;
    ;; START PROCEDURE
    ;;

    (define (show-splash! key-function)
      (visual 'show-splash!)
      ((window 'set-key-callback!) key-function))
    
    (define (start! update-function game)
      ((window 'set-background!) "black")
      ((window 'set-update-callback!) update-function)
      (visual 'start! (game 'level)))

    ;;
    ;; UPDATE PROCEDURE
    ;;
    
    (define (update! game)
       (visual 'update! game))

    (define (dispatch message . parameters)
      (cond ((eq? message 'start!) (apply start! parameters))
            ((eq? message 'update!) (apply update! parameters))
            ((eq? message 'show-splash!) (apply show-splash! parameters))
            ((eq? message 'game-over!) (visual 'game-over!))
            (else  (error "[ERROR in DrawADT DISPATCH] Wrong message: ") (display message))))

    dispatch))