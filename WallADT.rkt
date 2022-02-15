(define (make-wall position-object)
  (lambda (message . parameters)
    (cond
      ((eq? message 'position) position-object)
      ((eq? message 'kind) 'wall)
      (else  (error "[ERROR in WallADT DISPATCH] Wrong message: ") (display message)))))