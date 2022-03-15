(define (make-level level-number initial-ant-pos)
  (let ((walls '())
        (scorpion-time 0)
        (scorpions '())
        (eggs '())
        (ant (make-movingobject initial-ant-pos 'right 'ant))
        (update-score? #f)
        (remove-live? #f)
        (done? #f))


    ;;
    ;; ADD PROCEDURES
    ;;

    (define (add-walls lst)
      (for-each (lambda (positions) (add-multiple-walls (make-position (caar positions) (cadar positions))  (make-position (caadr positions) (cadadr positions)))) lst))
    
    (define (add-wall position-object)
      (set! walls (cons (make-wall (make-position (position-object 'x) (+ (position-object 'y) top-border))) walls)))

    (define (add-vertical-wall start-position end-position)
      (let build-wall ((count (start-position 'y))
                       (x (start-position 'x)))
        (if (= count (end-position 'y))
            (add-wall (make-position x count))
            (begin
              (add-wall (make-position x count))
              (build-wall (+ count 1) x)))))

    (define (add-horizontal-wall start-position end-position)
      (let build-wall ((count (start-position 'x))
                       (y (start-position 'y)))
        (if (= count (end-position 'x))
            (add-wall (make-position count y))
            (begin
              (add-wall (make-position count y))
              (build-wall (+ count 1) y)))))

    ;; The main procedure that makes a long wall, it checks if you want to build a vertical or a horzizontal wall
    ;; and then calls for seperate procedures to build those walls
    (define (add-multiple-walls position1 position2)
      (let ((x1 (position1 'x))
            (x2 (position2 'x))
            (y1 (position1 'y))
            (y2 (position2 'y)))
        (cond
          ((= x1 x2) (add-vertical-wall position1 position2))
          ((= y1 y2) (add-horizontal-wall position1 position2))
          (else (error "[ERROR in LevelADT add-multiple-walls] Cannot build walls diagonally!")))))

    (define (add-scorpions lst)
      (for-each (lambda (scorpion-info) (add-scorpion (make-position (car scorpion-info) (cadr scorpion-info)) (caddr scorpion-info))) lst))

    (define (add-scorpion position-object orientation)
      (set! scorpions (cons (make-movingobject position-object orientation 'scorpion) scorpions)))

    (define (add-eggs lst)
      (for-each (lambda (x-and-y) (add-egg (make-position (car x-and-y) (cadr x-and-y)))) lst))
    
    (define (add-egg position-object)
      (set! eggs (cons (make-egg position-object) eggs)))

    (define (initial-ant-pos! position-object)
      (set! initial-ant-pos position-object))

    ;;
    ;; COLLISION DETECTION
    ;;

    ;; Can a given object move in the given direction without obstruction?
    (define (can-move? moving-object direction kind)

      ;; Returns list with every wall it collides with
      (define (wall-collision-list direction)
        (map (lambda (wall-object) (upcomming-collision? wall-object direction)) walls))

      ;; Returns list with every egg it collides with
      (define (egg-collision-list direction)
        (map (lambda (egg-object) (collision? egg-object)) eggs))

      ;; Checks if there is going to be a collision when moving to given direction
      (define (upcomming-collision? object direction)
        (let ((current-x ((moving-object 'position) 'x))
              (current-y ((moving-object 'position) 'y)))
          (cond
            ((eq? direction 'right) ((object 'position) 'equal? (make-position (+ current-x 1) current-y)))
            ((eq? direction 'left) ((object 'position) 'equal? (make-position (- current-x 1) current-y)))
            ((eq? direction 'up) ((object 'position) 'equal? (make-position current-x (- current-y 1))))
            ((eq? direction 'down) ((object 'position) 'equal? (make-position current-x (+ current-y 1)))))))

      (define (collision? object)
        ((object 'position) 'equal? (moving-object 'position)))

      (cond
        ((eq? kind 'wall) (not (list? (member #t (wall-collision-list direction)))))
        ((eq? kind 'egg) (not (list? (member #t (egg-collision-list direction)))))))

    (define (remove-egg!)
      (let ((current-eggs (reverse eggs)))
        (define (look-for-remove current remaining result)
          (cond
            ((null? current-eggs) result)
            ((and (null? remaining) ((current 'position) 'equal? (ant 'position))) (set! eggs result))
            (((current 'position) 'equal? (ant 'position)) (set! eggs (append result remaining)))
            (else (look-for-remove (car remaining) (cdr remaining) (cons current result)))))
        (look-for-remove (car current-eggs) (cdr current-eggs) '())))

    ;; Checks if an egg needs to be removed
    (define (check-and-remove-eggs! direction)
      (if (not (can-move? ant direction 'egg))
          (begin
            (remove-egg!)
            (update-score! #t))))

    ;; Checks if any scorpion collides with the ant and moves the ant back to initial-ant-pos
    (define (check-for-ant-scorpion-collision)
      (if (list? (member #t (map (lambda (scorpion-object) ((ant 'position) 'equal? (scorpion-object 'position))) scorpions)))
        (begin
          (remove-live! #t)
          (reset))))

    ;;
    ;; RESET
    ;;

    (define (reset)
      (ant 'position! initial-ant-pos)
      )

    ;;
    ;; Lives
    ;;

    (define (remove-live! bool)
      (if bool
          (set! remove-live? #t)
          (set! remove-live? #f)))
    
    ;;
    ;; MOVING
    ;;

    (define (move-scorpion! delta-time)
      (define (move! scorpion)
        (let ((orientation (scorpion 'orientation)))
          (if (can-move? scorpion orientation 'wall)
              (begin
                (scorpion 'move! distance)
                (set! scorpion-time 0))
              (begin
                (scorpion 'set-opposite-orientation! orientation)
                (scorpion 'move! distance)
                (set! scorpion-time 0)))))
      (set! scorpion-time (+ scorpion-time delta-time))
      (if (> scorpion-time max-scorpion-time)
          (for-each move! scorpions)))

    (define (move-ant! key)
      (if (or (and (eq? key 'right) (can-move? ant key 'wall))
              (and (eq? key 'left) (can-move? ant key 'wall))
              (and (eq? key 'up) (can-move? ant key 'wall) (not (<= ((ant 'position) 'y) top-border)))
              (and (eq? key 'down) (can-move? ant key 'wall) (not (>= ((ant 'position) 'y) bottom-border))))
          (begin
            (ant 'orientation! key)
            (ant 'move! distance)
            (check-and-remove-eggs! key))))

    ;;
    ;; SCORE
    ;;

    (define (update-score! bool)
      (if bool
          (set! update-score? #t)
          (set! update-score? #f)))
    
    ;;
    ;; DATA ABSTRACTION PROCEDURES
    ;;
    
    (define (member? element object-list)
      (cond
        ((eq? object-list 'eggs) (member element eggs))
        (else (display "[ERROR in LevelADT member?] Wrong object-list") (display object-list))))

    (define (length? object-list)
      (cond
        ((eq? object-list 'eggs) (length eggs))
        (else (display "[ERROR in LevelADT length?] Wrong object-list") (display object-list))))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'add-walls) (apply add-walls parameters))
        ((eq? message 'walls) walls)
        ((eq? message 'add-scorpions) (apply add-scorpions parameters))
        ((eq? message 'scorpions) scorpions)
        ((eq? message 'add-eggs) (apply add-eggs parameters))
        ((eq? message 'eggs) eggs)
        ((eq? message 'initial-ant-pos!) (apply initial-ant-pos! parameters))
        ((eq? message 'ant) ant)
        ((eq? message 'move-ant!) (apply move-ant! parameters))
        ((eq? message 'move-scorpion!) (apply move-scorpion! parameters))
        ((eq? message 'check-for-ant-scorpion-collision) (check-for-ant-scorpion-collision))
        ((eq? message 'update-score?) update-score?)
        ((eq? message 'update-score!) (apply update-score! parameters))
        ((eq? message 'remove-live?) remove-live?)
        ((eq? message 'remove-live!) (apply remove-live! parameters))
        ((eq? message 'member?) (apply member? parameters))
        ((eq? message 'length?) (apply length? parameters))
        ((eq? message 'done?) done?)
        (else  (error "[ERROR in LevelADT DISPATCH] Wrong message: ") (display message))))

    dispatch))