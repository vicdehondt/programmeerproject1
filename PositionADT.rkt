(define (make-position x y)
  (let ((x-value x)
        (y-value y))

    (define (equal? position-object)
      (and (= x-value (position-object 'x)) (= y-value (position-object 'y))))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'x) x-value)
        ((eq? message 'y) y-value)
        ((eq? message 'equal?) (apply equal? parameters))
        (else (error "[ERROR in PositionADT DISPATCH] Wrong message: ") (display message))))
    
    dispatch))