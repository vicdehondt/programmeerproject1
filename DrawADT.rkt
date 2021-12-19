(define (make-draw)
  (let* ((window (make-window 456 360 "Fire Ant"))
         (visual (make-visual window)))
    
    ;;
    ;; Start procedure
    ;;
    
    (define (start! update-function key-function game-object)
      ((window 'set-background!) "black")
      ((window 'set-update-callback!) update-function)
      ((window 'set-key-callback!) key-function)
      ((visual 'start!) (game-object 'level)))

    ;;
    ;; Update procedure
    ;;
    
    (define (update! game-object)
      (update-level! (game-object 'level)))
    
    (define (update-level! level-object)
      ((visual 'update!) level-object))

    (define (dispatch msg)
      (cond ((eq? msg 'start!) start!)
            ((eq? msg 'update!) update!)))
    dispatch))