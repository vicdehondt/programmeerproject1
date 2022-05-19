(define (make-moving-object position orientation kind)
  (let ((previous-orientation orientation))

    (define (position! new-position-object)
      (set! position new-position-object))

    (define (set-opposite-orientation!)
        (cond
          ((eq? orientation 'right) (orientation! 'left))
          ((eq? orientation 'left) (orientation! 'right))
          ((eq? orientation 'up) (orientation! 'down))
          ((eq? orientation 'down) (orientation! 'up))
          (else (error "[ERROR in MovingObjectADT set-opposite-orientation!] Wrong orientation!"))))

    (define (set-random-orientation!)
      (let ((orientations (vector 'left 'right 'up 'down)))
        (orientation! (vector-ref orientations (random 0 4)))))

    (define (orientation! new-orientation)
      (set! orientation new-orientation))

    (define (previous-orientation! given-orientation)
      (set! previous-orientation given-orientation))

    (define (new-orientation!)
      (cond
        ((eq? kind 'normal-scorpion) (set-opposite-orientation!))
        ((eq? kind 'random-scorpion) (set-random-orientation!))
        (else (error "[ERROR in MovingObjectADT new-orientation!] Wrong kind!"))))
    
    (define (move! distance)
      (cond
        ((eq? orientation 'right) (position! (make-position (+ (position 'x) distance) (position 'y))))
        ((eq? orientation 'left) (position! (make-position (- (position 'x) distance) (position 'y))))
        ((eq? orientation 'up) (position! (make-position (position 'x) (- (position 'y) distance))))
        ((eq? orientation 'down) (position! (make-position (position 'x) (+ (position 'y) distance))))
        (error "[ERROR in MovingObjectADT move!] Wrong orientation!")))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'position) position)
        ((eq? message 'position!) (apply position! parameters))
        ((eq? message 'orientation) orientation)
        ((eq? message 'orientation!) (apply orientation! parameters))
        ((eq? message 'set-opposite-orientation!) (apply set-opposite-orientation! parameters))
        ((eq? message 'new-orientation!) (apply new-orientation! parameters))
        ((eq? message 'previous-orientation) previous-orientation)
        ((eq? message 'previous-orientation!) (apply previous-orientation! parameters))
        ((eq? message 'kind) kind)
        ((eq? message 'move!) (apply move! parameters))
        (else  (error "[ERROR in MovingObjectADT DISPATCH] Wrong message!"))))
    
    dispatch))
