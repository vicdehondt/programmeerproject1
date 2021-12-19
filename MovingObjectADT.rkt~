(define (make-movingobject position orientation kind)

  (define (position! new-position-object)
    (set! position new-position-object))

  (define (orientation! new-orientation)
    (set! orientation new-orientation))

  (define (move! distance)
    (cond
      ((eq? orientation 'right) (position! (make-position (+ (position 'x) distance) (position 'y))))
      ((eq? orientation 'left) (position! (make-position (- (position 'x) distance) (position 'y))))
      ((eq? orientation 'up) (position! (make-position (position 'x) (- (position 'y) distance))))
      ((eq? orientation 'down) (position! (make-position (position 'x) (+ (position 'y) distance))))))

  (define (dispatch m)
    (cond
      ((eq? m 'position) position)
      ((eq? m 'position!) position!)
      ((eq? m 'orientation) orientation)
      ((eq? m 'orientation!) orientation!)
      ((eq? m 'kind) kind)
      ((eq? m 'move!) move!)
      (else (error "ERROR in DISPATCH: Wrong message!"))))
  dispatch)
