(define (make-wall position-object)
  
    (define (dispatch m)
      (cond
        ((eq? m 'position) position-object)
        ((eq? m 'kind) 'wall)
        (else  (error "[ERROR in WallADT DISPATCH] Wrong message: ") (display m))))
    dispatch)