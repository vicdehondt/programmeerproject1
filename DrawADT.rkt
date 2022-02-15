(define (make-draw)
  (let* ((window (make-window 912 720 "Fire Ant"))
         (visual (make-visual window)))
    
    ;;
    ;; Start procedure
    ;;
    
    (define (start! update-function key-function level-object)
      ((window 'set-background!) "black")
      ((window 'set-update-callback!) update-function)
      ((window 'set-key-callback!) key-function)
      (visual 'start! level-object))

    ;;
    ;; Update procedure
    ;;
    
    (define (update! level-object)
      (update-level! level-object))
    
    (define (update-level! level-object)
      (visual 'update! level-object))

    (lambda (message . parameters)
      (cond ((eq? message 'start!) (apply start! parameters))
            ((eq? message 'update!) (apply update! parameters))
            (else  (error "[ERROR in DrawADT DISPATCH] Wrong message: ") (display message))))))