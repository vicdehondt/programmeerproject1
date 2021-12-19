(define (make-movingobject position orientation kind)
  (let ((old-orientation orientation))

    ;; Give the object a new position
    (define (position! new-position-object)
      (set! position new-position-object))

    ;; Give the object a new orientation
    (define (orientation! new-orientation)
      (set! orientation new-orientation))

    ;; Update the old-orientation
    (define (old-orientation! given-orientation)
      (set! old-orientation given-orientation))

    ;; Move the object a given distance
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
        ((eq? m 'old-orientation) old-orientation)
        ((eq? m 'old-orientation!) old-orientation!)
        ((eq? m 'kind) kind)
        ((eq? m 'move!) move!)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))
