
(define (make-level level-number initial-ant-pos)
  (let ((scorpion-time 0)
        (walls '())
        (scorpions '())
        (eggs '())
        (ant (make-movingobject initial-ant-pos 'right 'ant))
        (done? #f))

    ;; Add a wall to the level
    (define (add-wall position-object)
      (set! walls (cons (make-wall position-object) walls)))

    ;; Add a scorpion to the level
    (define (add-scorpion position-object)
      (set! scorpions (cons (make-movingobject position-object 'right 'scorpion) scorpions)))

    ;; Add an egg to the level
    (define (add-egg position-object)
      (set! eggs (cons (make-egg position-object) eggs)))

    ;; Set the beginpoint of the ant
    (define (initial-ant-pos! position-object)
      (set! initial-ant-pos position-object))

    ;;
    ;; COLLISION DETECTION
    ;;

    ;; Is the next position free?
    (define (free? moving-object direction kind)

      ;; Gives list with every wall it collides with
      (define (wall-collision-list direction)
        (for-each-object (lambda (wall-object) (upcomming-collision? wall-object direction)) walls))

      ;; Gives list with every egg it collides with
      (define (egg-collision-list direction)
        (for-each-object (lambda (egg-object) (collision? egg-object)) eggs))

      ;; Checks if there is going to be a collision
      (define (upcomming-collision? object direction)
        (let ((current-x ((moving-object 'position) 'x))
              (current-y ((moving-object 'position) 'y)))
          (cond
            ((eq? direction 'right) (((object 'position) 'equal?) (make-position (+ current-x 1) current-y)))
            ((eq? direction 'left) (((object 'position) 'equal?) (make-position (- current-x 1) current-y)))
            ((eq? direction 'up) (((object 'position) 'equal?) (make-position current-x (- current-y 1))))
            ((eq? direction 'down) (((object 'position) 'equal?) (make-position current-x (+ current-y 1)))))))

      ;; Checks if there is a collision at the moment
      (define (collision? object)
        (((object 'position) 'equal?) (moving-object 'position)))

      (cond
        ((eq? kind 'wall) (not (list? (member #t (wall-collision-list direction)))))
        (else (not (list? (member #t (egg-collision-list direction)))))))

    ;; Looks for egg to remove and removes it
    (define (remove-egg!)
      (let ((lst (reverse eggs)))
        (define (look-for-remove current remaining result)
          (cond
            ((null? lst) result)
            ((and (null? remaining) (((current 'position) 'equal?) (ant 'position))) (set! eggs result))
            ((((current 'position) 'equal?) (ant 'position)) (set! eggs (append result remaining)))
            (else (look-for-remove (car remaining) (cdr remaining) (cons current result)))))
        (look-for-remove (car lst) (cdr lst) '())))

    ;; Checks if an egg needs to be removed
    (define (check-eggs! key)
      (if (not (free? ant key 'egg))
          (begin
            (remove-egg!))))
    
    ;;
    ;; Move
    ;;

    ;; Moves a scorpion every ... milliseconds
    (define (move-scorpion! delta-time)

      ;; Looks for opposite orientation to turn around
      (define (opposite orientation)
        (cond
          ((eq? orientation 'right) 'left)
          ((eq? orientation 'left) 'right)
          ((eq? orientation 'up) 'down)
          ((eq? orientation 'down) 'up)))

      ;; Moves the scorpion
      (define (move! scorpion)
        (if (free? scorpion (scorpion 'orientation) 'wall)
            (begin
              ((scorpion 'move!) 1)
              (set! scorpion-time 0))
            (begin
              ((scorpion 'orientation!) (opposite (scorpion 'orientation)))
              ((scorpion 'move!) 1)
              (set! scorpion-time 0))))
      (set! scorpion-time (+ scorpion-time delta-time))
      (if (> scorpion-time 600)
          (for-each-object move! scorpions)))

    ;; Moves the ant
    (define (move-ant! key)
      (if (or (and (eq? key 'right) (free? ant key 'wall))
              (and (eq? key 'left) (free? ant key 'wall))
              (and (eq? key 'up) (free? ant key 'wall) (not (<= ((ant 'position) 'y) top-border)))
              (and (eq? key 'down) (free? ant key 'wall) (not (>= ((ant 'position) 'y) bottom-border))))
          (begin
            ((ant 'orientation!) key)
            ((ant 'move!) 1)
            (check-eggs! key))))
  
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
        ((eq? m 'check-eggs!) check-eggs!)
        ((eq? m 'done?) done?)
        (else (error "ERROR in DISPATCH: Wrong message!"))))
    dispatch))