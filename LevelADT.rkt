
(define (make-level level-number initial-ant-pos)
  (let ((scorpion-time 0)
        (walls '())
        (scorpions '())
        (eggs '())
        (ant (make-movingobject initial-ant-pos 'right 'ant))
        (done? #f))
    #|
    (define (add-wall start-position end-position orientation)
      (define (iter-vertical remaining)
        (if (eq? remaining (end-position 'y))
            (add-wall-piece (end-position))
            (begin
              (add-wall-piece (make-position (end-position 'x) remaining))
              (iter-vertical (+ remaining 1)))))

      (define (iter-horizontal remaining)
        (if (eq? remaining (end-position 'x))
            (add-wall-piece (end-position))
            (begin
              (add-wall-piece (make-position remaining (end-position 'y)))
              (iter-horizontal (+ remaining 1)))))
      
      (if (eq? orientation 'vertical)
          (iter-vertical (start-position 'y))
          (iter-horizontal (start-position 'x))))

    (define (add-wall-piece position-object)
      (set! walls (cons (make-wall position-object) walls)))
    |#

    (define (add-wall position-object)
      (set! walls (cons (make-wall position-object) walls)))

    (define (add-scorpion position-object)
      (set! scorpions (cons (make-movingobject position-object 'right 'scorpion) scorpions)))

    (define (add-egg position-object)
      (set! eggs (cons (make-egg position-object) eggs)))

    (define (initial-ant-pos! position-object)
      (set! initial-ant-pos position-object))

    ;;
    ;; Collision Detection
    ;;
    
    (define (free? moving-object direction)

      (define (collision-list direction)
        (for-each-object (lambda (wall-object) (collision? wall-object direction)) walls))

      (define (collision? wall-object direction)
        (let ((current-x ((moving-object 'position) 'x))
              (current-y ((moving-object 'position) 'y)))
          (cond
            ((eq? direction 'right) (((wall-object 'position) 'equal?) (make-position (+ current-x 1) current-y)))
            ((eq? direction 'left) (((wall-object 'position) 'equal?) (make-position (- current-x 1) current-y)))
            ((eq? direction 'up) (((wall-object 'position) 'equal?) (make-position current-x (- current-y 1))))
            ((eq? direction 'down) (((wall-object 'position) 'equal?) (make-position current-x (+ current-y 1)))))))
      
      (not (list? (member #t (collision-list direction)))))


    ;;
    ;; Move
    ;;
    
    (define (move-scorpion! delta-time)

      (define (opposite orientation)
        (cond
          ((eq? orientation 'right) 'left)
          ((eq? orientation 'left) 'right)
          ((eq? orientation 'up) 'down)
          ((eq? orientation 'down) 'up)))

      (define (move! scorpion)
        (if (and (> scorpion-time 1000) (free? scorpion (scorpion 'orientation)))
            (begin
              ;; Move scorpion 1 in current direction
              ((scorpion 'move!) 1)
              (display ((scorpion 'position) 'x))
              ;; Reset the timer.
              (set! scorpion-time 0))
            (begin
              ((scorpion 'orientation!) (opposite (scorpion 'orientation)))
              ((scorpion 'move!) 1)
              (display ((scorpion 'position) 'x))
              ;; Reset the timer.
              (set! scorpion-time 0))))
      (set! scorpion-time (+ scorpion-time delta-time))
      (for-each-object move! scorpions))

    (define (move-ant! key)
      (if (or (and (eq? key 'right) (free? ant key))
              (and (eq? key 'left) (free? ant key))
              (and (eq? key 'up) (free? ant key))
              (and (eq? key 'down) (free? ant key)))
          (begin
            ((ant 'orientation!) key)
            ((ant 'move!) 1))))
    
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
        ((eq? m 'move-ant!) move-ant!)
        ((eq? m 'move-scorpion!) move-scorpion!)
        ((eq? m 'done?) done?)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))