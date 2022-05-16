(define (make-stationary-object position-object kind)
  
  (define (dispatch message . parameters)
    (cond
      ((eq? message 'position) position-object)
      ((eq? message 'kind) kind)
      (else  (error "[ERROR in StationaryObjectADT DISPATCH] Wrong message!"))))
    
  dispatch)