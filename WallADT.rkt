(define (make-wall position-object)

  (define (dispatch message . parameters)
      (cond
      ((eq? message 'position) position-object)
      ((eq? message 'kind) 'wall)
      (else  (error "[ERROR in WallADT DISPATCH] Wrong message: ") (display message))))
  
  dispatch)