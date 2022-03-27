(define (make-powerup position-object kind)
  (let ((position position-object))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'position) position)
        ((eq? message 'kind) kind)
        (else  (error "[ERROR in PuzzleObjectADT DISPATCH] Wrong message!"))))
    
    dispatch))