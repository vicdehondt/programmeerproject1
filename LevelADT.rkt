(load "PositionADT.rkt")
(load "MovingObjectADT.rkt")
(load "EggADT.rkt")
(load "WallADT.rkt")
(#%require (only racket error))

(define (make-level level-number)
  (let ((walls '())
        (scorpions '())
        (eggs '())
        (initial-ant-pos (make-position 0 0))
        (done? #f))
    
    (define (add-wall wall-object)
      (set! walls (cons wall-object walls)))

    (define (add-scorpion scorpion-object)
      (set! scorpions (cons scorpion-object scorpions)))

    (define (add-wegg egg-object)
      (set! eggs (cons egg-object eggs)))

    (define (initial-ant-pos! position-object)
      (set! initial-ant-pos position-object))
  
    (define (dispatch m)
      (cond
        ((eq? m 'add-wall) add-wall)
        ((eq? m 'add-scorpion) add-scorpion)
        ((eq? m 'add-egg) add-egg)
        ((eq? m 'initial-ant-pos!) initial-ant-pos!)
        ((eq? m 'done?) done?)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))