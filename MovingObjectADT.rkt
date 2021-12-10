
(define (make-movingobject position-object orientation)
  (let ((position position-object)
        (orientation orientation))

    (define (position! new-position-object)
      (set! position new-position-object))

    (define (orientation! new-orientation)
      (set! orientation new-orientation))

    (define (move-left distance)
      (orientation! 'left)
      (position! (make-position (- (position 'x) distance) (position 'y))))

    (define (move-right distance)
      (orientation! 'right)
      (position! (make-position (+ (position 'x) distance) (position 'y))))

    (define (move-up distance)
      (orientation! 'up)
      (position! (make-position (position 'x) (- (position 'y) distance))))

    (define (move-down distance)
      (orientation! 'down)
      (position! (make-position (position 'x) (+ (position 'y) distance))))
    
    (define (dispatch m)
      (cond
        ((eq? m 'position) position)
        ((eq? m 'position!) position!)
        ((eq? m 'orientation) orientation)
        ((eq? m 'orientation!) orientation!)
        ((eq? m 'move-left) move-left)
        ((eq? m 'move-right) move-right)
        ((eq? m 'move-up) move-up)
        ((eq? m 'move-down) move-down)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))
