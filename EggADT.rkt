(define (make-egg position-object)
  (let ((position position-object))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'position) position)
        ((eq? message 'kind) 'egg)
        (else  (error "[ERROR in EggADT DISPATCH] Wrong message: ") (display message))))
    
    dispatch))