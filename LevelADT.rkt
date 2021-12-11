
(define (make-level level-number initial-ant-pos)
  (let ((walls '())
        (scorpions '())
        (eggs '())
        (ant (make-movingobject initial-ant-pos 'right))
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

    (define (for-each-object f object-list)
      (map f object-list))
  
    (define (dispatch m)
      (cond
        ((eq? m 'add-wall) add-wall)
        ((eq? m 'walls) walls)
        ((eq? m 'add-scorpion) add-scorpion)
        ((eq? m 'scorpions) scorpions)
        ((eq? m 'add-egg) add-egg)
        ((eq? m 'eggs) eggs)
        ((eq? m 'for-each-object) for-each-object)
        ((eq? m 'initial-ant-pos!) initial-ant-pos!)
        ((eq? m 'ant) ant)
        ((eq? m 'move!) move!)
        ((eq? m 'done?) done?)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))