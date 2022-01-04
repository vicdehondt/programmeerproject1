(define (make-draw)
  (let* ((window (make-window 912 720 "Fire Ant"))
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

    (define (dispatch m)
      (cond ((eq? m 'start!) start!)
            ((eq? m 'update!) update!)
            (else  (error "[ERROR in DrawADT DISPATCH] Wrong message: ") (display m))))
    dispatch))