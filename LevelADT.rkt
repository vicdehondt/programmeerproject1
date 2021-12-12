
(define (make-level level-number initial-ant-pos)
  (let ((walls '())
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
    
    (define (free? direction)

      (define (collision-list direction)
        (for-each-object (lambda (wall-object) (collision? wall-object direction)) walls))

      (define (collision? wall-object direction)
        (let ((current-x ((ant 'position) 'x))
              (current-y ((ant 'position) 'y)))
        (cond
          ((eq? direction 'right) (((wall-object 'position) 'equal?) (make-position (+ current-x 1) current-y)))
          ((eq? direction 'left) (((wall-object 'position) 'equal?) (make-position (- current-x 1) current-y)))
          ((eq? direction 'up) (((wall-object 'position) 'equal?) (make-position current-x (- current-y 1))))
          ((eq? direction 'down) (((wall-object 'position) 'equal?) (make-position current-x (+ current-y 1)))))))
      
      (not (list? (member #t (collision-list direction)))))


    ;;
    ;; Move
    ;;
    
    (define (move! key)
      (cond
        ((and (eq? key 'right) (free? 'right))
         ((ant 'move-right) 1))
        ((and (eq? key 'left) (free? 'left))
         ((ant 'move-left) 1))
        ((and (eq? key 'up) (free? 'up))
         ((ant 'move-up) 1))
        ((and (eq? key 'down) (free? 'down))
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