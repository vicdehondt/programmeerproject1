(define (make-level level-number initial-ant-pos top-border bottom-border)
  (let ((walls '())
        (scorpion-time 0)
        (scorpions '())
        (keys '())
        (doors '())
        (power-ups '())
        (eggs '())
        (ant (make-movingobject initial-ant-pos 'right 'ant))
        (update-score? #f)
        (remove-live? #f)
        (inventory '((empty) (empty) (empty)))
        (speed-up? #f)
        (done? #f))


    ;;
    ;; ADD PROCEDURES
    ;;

    (define (add-walls lst)
      (for-each (lambda (positions) (add-multiple-walls (make-position (caar positions) (cadar positions))  (make-position (caadr positions) (cadadr positions)))) lst))
    
    (define (add-wall position-object)
      (set! walls (cons (make-wall (make-position (position-object 'x) (position-object 'y))) walls)))

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

    (define (add-puzzleobjects lst)
      (for-each (lambda (x-y-kind) (add-puzzleobject (make-position (car x-y-kind) (cadr x-y-kind)) (caddr x-y-kind))) lst))

    (define (add-puzzleobject position-object kind)
      (cond
        ((eq? kind 'key) (set! keys (cons (make-puzzleobject position-object kind) keys)))
        ((eq? kind 'door) (set! doors (cons (make-puzzleobject position-object kind) doors)))))

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

      (define (door-collision-list direction)
        (map (lambda (door-object) (upcomming-collision? door-object direction)) doors))

      (define (key-collision-list direction)
        (map (lambda (key-object) (collision? key-object)) keys))

      ;; Returns list with every egg it collides with
      (define (egg-collision-list direction)
        (map (lambda (egg-object) (collision? egg-object)) eggs))

      (define (next-position)
        (let ((current-x ((moving-object 'position) 'x))
              (current-y ((moving-object 'position) 'y)))
          (cond
            ((eq? direction 'right) (make-position (+ current-x 1) current-y))
            ((eq? direction 'left) (make-position (- current-x 1) current-y))
            ((eq? direction 'up) (make-position current-x (- current-y 1)))
            ((eq? direction 'down) (make-position current-x (+ current-y 1))))))
      
      ;; Checks if there is going to be a collision when moving to given direction
      (define (upcomming-collision? object direction)
        ((object 'position) 'equal? (next-position)))

      (define (collision? object)
        ((object 'position) 'equal? (moving-object 'position)))

      (define (check-for-opening)
        (cond
          ((and (list? (member #t (door-collision-list direction))) (eq? (caar inventory) 'key)) (remove-door!)
                                                                                                 #t)
          ((and (list? (member #t (door-collision-list direction))) (eq? (caar inventory) 'empty)) #f)
          (else #t)))

    (define (remove-door!)
      (let ((current-doors (reverse doors)))
        (define (look-for-remove current remaining result)
          (set-car! inventory (cdar inventory))
          (cond
            ((null? current-doors) result)
            ((and (null? remaining) ((current 'position) 'equal? (next-position))) (set! doors result))
            (((current 'position) 'equal? (next-position)) (set! doors (append result remaining)))
            (else (look-for-remove (car remaining) (cdr remaining) (cons current result)))))
        (look-for-remove (car current-doors) (cdr current-doors) '())))

      (cond
        ((eq? kind 'wall) (not (list? (member #t (wall-collision-list direction)))))
        ((eq? kind 'door) (if (eq? (moving-object 'kind) 'ant) (check-for-opening)
                              (not (list? (member #t (door-collision-list direction))))))
        ((eq? kind 'egg) (not (list? (member #t (egg-collision-list direction)))))
        ((eq? kind 'key) (not (list? (member #t (key-collision-list direction)))))))

    (define (remove-egg!)
      (let ((current-eggs (reverse eggs)))
        (define (look-for-remove current remaining result)
          (cond
            ((null? current-eggs) result)
            ((and (null? remaining) ((current 'position) 'equal? (ant 'position))) (set! eggs result))
            (((current 'position) 'equal? (ant 'position)) (set! eggs (append result remaining)))
            (else (look-for-remove (car remaining) (cdr remaining) (cons current result)))))
        (look-for-remove (car current-eggs) (cdr current-eggs) '())))

    (define (add-to-inventory! object)
      (cond
        ((eq? (object 'kind) 'key) (set-car! inventory (cons 'key (car inventory))))
        (else (display "[ERROR in LevelADT add-to-inventory!] Wrong kind given!") (display (object 'kind)))))

    (define (remove-and-add-to-inv!)
      (let ((current-keys (reverse keys)))
        (define (look-for-remove current remaining result)
          (cond
            ((null? current-keys) result)
            ((and (null? remaining) ((current 'position) 'equal? (ant 'position))) (add-to-inventory! current)
                                                                                   (set! keys result))
            (((current 'position) 'equal? (ant 'position)) (add-to-inventory! current)
                                                           (set! keys (append result remaining)))
            (else (look-for-remove (car remaining) (cdr remaining) (cons current result)))))
        (look-for-remove (car current-keys) (cdr current-keys) '())))

    ;; Checks if an egg needs to be removed
    (define (check-and-remove-eggs! direction)
      (if (not (can-move? ant direction 'egg))
          (begin
            (remove-egg!)
            (update-score! #t))))

    ;; Check if the ant collides with a key
    (define (check-for-key direction)
      (if (not (can-move? ant direction 'key))
          (begin
            (remove-and-add-to-inv!))))

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
      (let ((interval (if speed-up? speed-interval normal-interval)))
      (define (move! scorpion)
        (let ((orientation (scorpion 'orientation)))
          (if (and (can-move? scorpion orientation 'wall) (can-move? scorpion orientation 'door))
              (begin
                (scorpion 'move! distance)
                (set! scorpion-time 0))
              (begin
                (scorpion 'set-opposite-orientation! orientation)
                (scorpion 'move! distance)
                (set! scorpion-time 0)))))
      (set! scorpion-time (+ scorpion-time delta-time))
      (if (> scorpion-time interval)
          (for-each move! scorpions))))

    (define (move-ant! key)
      (if (or (and (eq? key 'right) (can-move? ant key 'wall) (can-move? ant key 'door))
              (and (eq? key 'left) (can-move? ant key 'wall) (can-move? ant key 'door))
              (and (eq? key 'up) (can-move? ant key 'wall) (can-move? ant key 'door) (not (<= ((ant 'position) 'y) top-border)))
              (and (eq? key 'down) (can-move? ant key 'wall) (can-move? ant key 'door) (not (>= ((ant 'position) 'y) bottom-border))))
          (begin
            (ant 'orientation! key)
            (ant 'move! distance)
            (check-and-remove-eggs! key)
            (check-for-key key))))

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
        ((eq? object-list 'egg) (member element eggs))
        ((eq? object-list 'key) (member element keys))
        ((eq? object-list 'door) (member element doors))
        (else (display "[ERROR in LevelADT member?] Wrong object-list") (display object-list))))

    (define (length? object-list)
      (cond
        ((eq? object-list 'egg) (length eggs))
        ((eq? object-list 'key) (length keys))
        ((eq? object-list 'door) (length doors))
        (else (display "[ERROR in LevelADT length?] Wrong object-list") (display object-list))))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'add-walls) (apply add-walls parameters))
        ((eq? message 'walls) walls)
        ((eq? message 'add-scorpions) (apply add-scorpions parameters))
        ((eq? message 'scorpions) scorpions)
        ((eq? message 'add-eggs) (apply add-eggs parameters))
        ((eq? message 'eggs) eggs)
        ((eq? message 'add-puzzleobjects) (apply add-puzzleobjects parameters))
        ((eq? message 'keys) keys)
        ((eq? message 'inventory) inventory)
        ((eq? message 'doors) doors)
        ((eq? message 'add-powerups) (apply add-powerups parameters))
        ((eq? message 'power-ups) power-ups)
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