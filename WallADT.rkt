(define (make-wall position-object)
    
    (define (free? position-2nd-object)
      ((position-object 'equal?) position-2nd-object))
  
    (define (dispatch m)
      (cond
        ((eq? m 'position) position-object)
        ((eq? m 'free?) free?)
        ((eq? m 'kind) 'wall)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch)