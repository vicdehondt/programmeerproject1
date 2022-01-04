(define (make-egg position-object)
  (let ((position position-object))
    
    (define (dispatch m)
      (cond
        ((eq? m 'position) position)
        ((eq? m 'kind) 'egg)
        (else  (error "[ERROR in EggADT DISPATCH] Wrong message: ") (display m))))
    dispatch))