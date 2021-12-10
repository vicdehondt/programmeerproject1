
(define (make-level level-number)
  (let* ((walls '())
        (scorpions '())
        (eggs '())
        (initial-ant-pos (make-position 0 0))
        (ant (make-movingobject (make-position 0 0) 'right))
        (done? #f))
    
    (define (add-wall wall-object)
      (set! walls (cons wall-object walls)))

    (define (add-scorpion scorpion-object)
      (set! scorpions (cons scorpion-object scorpions)))

    (define (add-egg egg-object)
      (set! eggs (cons egg-object eggs)))

    (define (initial-ant-pos! position-object)
      (set! initial-ant-pos position-object))

    (define (move! key)
      (cond
        ((eq? key 'right)
         ((ant 'move-right) 1))
        ((eq? key 'left)
         ((ant 'move-left) 1))
        ((eq? key 'up)
         ((ant 'move-up) 1))
        ((eq? key 'down)
         ((ant 'move-down) 1))))
  
    (define (dispatch m)
      (cond
        ((eq? m 'add-wall) add-wall)
        ((eq? m 'add-scorpion) add-scorpion)
        ((eq? m 'add-egg) add-egg)
        ((eq? m 'initial-ant-pos!) initial-ant-pos!)
        ((eq? m 'ant) ant)
        ((eq? m 'move!) move!)
        ((eq? m 'done?) done?)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))