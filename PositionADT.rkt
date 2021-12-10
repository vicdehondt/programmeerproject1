(define (make-position x y)
  (let ((x-value x)
        (y-value y))

    (define (x! new-x)
      (set! x-value new-x))

    (define (y! new-y)
      (set! y-value new-y))

    (define (equal? position-object)
      (and (= x-value (position-object 'x)) (= y-value (position-object 'y))))

    (define (distance point-a point-b)
      (abs (- point-a point-b)))
    
    (define (horizontal-distance position-object)
      (distance x-value (position-object 'x)))

    (define (vertical-distance position-object)
      (distance y-value (position-object 'y)))

    (define (set-x-y! new-x new-y)
      (begin
        (x! new-x)
        (y! new-y)))

    (define (dispatch m)
      (cond
        ((eq? m 'x) x-value)
        ((eq? m 'y) y-value)
        ((eq? m 'x!) x!)
        ((eq? m 'y!) y!)
        ((eq? m 'equal?) equal?)
        ((eq? m 'horizontal-distance) horizontal-distance)
        ((eq? m 'vertical-distance) vertical-distance)
        ((eq? m 'set-x-y!) set-x-y!)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))