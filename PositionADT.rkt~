(define (make-position x y)
  (let ((x-value x)
        (y-value y))

    (define (equal? position-object)
      (and (= x-value (position-object 'x)) (= y-value (position-object 'y))))

    (define (dispatch m)
      (cond
        ((eq? m 'x) x-value)
        ((eq? m 'y) y-value)
        ((eq? m 'equal?) equal?)
        (else (error "[ERROR in PositionADT DISPATCH] Wrong message: ") (display m))))
    dispatch))