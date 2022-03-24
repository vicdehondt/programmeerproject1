(define (make-level level-number initial-ant-pos top-border bottom-border end-point)
  (let ((walls '())
        (scorpion-time 0)
        (normal-scorpions '())
        (random-scorpions '())
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
      (for-each (lambda (scorpion-info) (add-scorpion (make-position (list-ref scorpion-info 0) (list-ref scorpion-info 1)) (list-ref scorpion-info 2) (list-ref scorpion-info 3))) lst))

    (define (add-scorpion position-object orientation kind)
      (cond
        ((eq? kind 'normal) (set! normal-scorpions (cons (make-movingobject position-object orientation 'normal-scorpion) normal-scorpions)))
        ((eq? kind 'random) (set! random-scorpions (cons (make-movingobject position-object orientation 'random-scorpion) random-scorpions)))
        (else (error "[ERROR in LevelADT add-multiple-walls] Cannot build walls diagonally!"))))

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

    ;; Gives moving-object's next position based on the direction given
    (define (next-position moving-object direction)
      (let ((current-x ((moving-object 'position) 'x))
            (current-y ((moving-object 'position) 'y)))
        (cond
          ((eq? direction 'right) (make-position (+ current-x 1) current-y))
          ((eq? direction 'left) (make-position (- current-x 1) current-y))
          ((eq? direction 'up) (make-position current-x (- current-y 1)))
          ((eq? direction 'down) (make-position current-x (+ current-y 1))))))

    ;; Checks if there is going to be a collision when moving to given direction
    (define (upcomming-collision? moving-object position direction)
      (position 'equal? (next-position moving-object direction)))

    (define (collision? moving-object object)
      ((object 'position) 'equal? (moving-object 'position)))

    (define (within-border? moving-object)
      (or (not (<= ((moving-object 'position) 'y) top-border))
          (not (>= ((moving-object 'position) 'y) bottom-border))))


    ;; Gives list back with #t (collision) and #f (no obstruction)
    (define (egg-collision-list moving-object)
      (map (lambda (egg-object) (collision? moving-object egg-object)) eggs))

    (define (wall-collision-list moving-object direction)
      (map (lambda (wall-object) (upcomming-collision? moving-object (wall-object 'position) direction)) walls))

    (define (door-collision-list moving-object direction)
      (map (lambda (door-object) (upcomming-collision? moving-object (door-object 'position) direction)) doors))

    (define (key-collision-list moving-object)
      (map (lambda (key-object) (collision? moving-object key-object)) keys))


    ;; Can a given object move in the given direction without obstruction? (main collision-detection procedure)
    (define (can-move? moving-object direction kind)

      (define (check-for-key)
        (cond
          ((and (list? (member #t (door-collision-list moving-object direction))) (eq? (caar inventory) 'key)) (remove-door! direction) #t)
          ((and (list? (member #t (door-collision-list moving-object direction))) (eq? (caar inventory) 'empty)) #f)
          (else #t)))

      (cond
        ((eq? kind 'wall) (not (list? (member #t (wall-collision-list moving-object direction)))))
        ((eq? kind 'door) (if (eq? (moving-object 'kind) 'ant) (check-for-key)
                              (not (list? (member #t (door-collision-list moving-object direction))))))
        ((eq? kind 'egg) (not (list? (member #t (egg-collision-list moving-object)))))
        ((eq? kind 'key) (not (list? (member #t (key-collision-list moving-object)))))
        ((eq? kind 'border) (upcomming-collision? moving-object initial-ant-pos direction))))

    (define (remove-door! direction)
      (let ((current-doors (reverse doors)))
        (remove-from-inventory! 'key)
        (define (look-for-remove current remaining result)
          (cond
            ((null? current-doors) result)
            ((and (null? remaining) ((current 'position) 'equal? (next-position ant direction))) (set! doors result))
            (((current 'position) 'equal? (next-position ant direction)) (set! doors (append result remaining)))
            (else (look-for-remove (car remaining) (cdr remaining) (cons current result)))))
        (look-for-remove (car current-doors) (cdr current-doors) '())))

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
        (else (error "[ERROR in LevelADT add-to-inventory!] Wrong kind given!") (display (object 'kind)))))

    (define (remove-from-inventory! kind)
      (let ((keys (car inventory)))
        (cond
          ((eq? kind 'key) (set-car! inventory (cdr keys)))
          (else (error "[ERROR in LevelADT remove-from-inventory!] Wrong kind given!") (display (object 'kind))))))

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
      (define (ant-scorpion-collision? lst)
        (list? (member #t (map (lambda (scorpion-object) ((ant 'position) 'equal? (scorpion-object 'position))) lst))))
      (if (or (ant-scorpion-collision? normal-scorpions)
              (ant-scorpion-collision? random-scorpions))
          (begin
            (remove-live! #t)
            (reset))))

    ;;
    ;; RESET
    ;;

    (define (reset)
      (ant 'position! initial-ant-pos))

    (define (give-back! lst)
      (if (not (null? lst))
          (cond
            ((eq? ((car lst) 'kind) 'key) (set! keys (cons (car lst) keys))
                                          (give-back! (cdr lst)))
            ((eq? ((car lst) 'kind) 'door) (set! doors (cons (car lst) doors))
                                           (give-back! (cdr lst)))
            ((eq? ((car lst) 'kind) 'egg) (set! eggs (cons (car lst) eggs))
                                          (give-back! (cdr lst))))))

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
        (define (move-random! scorpion)
          (scorpion 'new-orientation!)
          (let ((orientation (scorpion 'orientation)))
          (if (and (can-move? scorpion orientation 'wall) (can-move? scorpion orientation 'door) (not (< ((next-position scorpion orientation) 'y) top-border)))
              (scorpion 'move! distance)
              (move-random! scorpion))))
        (define (move! scorpion)
          (let ((orientation (scorpion 'orientation)))
            (if (and (can-move? scorpion orientation 'wall) (can-move? scorpion orientation 'door) (not (< ((next-position scorpion orientation) 'y) top-border)))
                (begin
                  (scorpion 'move! distance)
                  (set! scorpion-time 0))
                (cond
                  ((eq? (scorpion 'kind) 'normal-scorpion) (scorpion 'new-orientation!) (scorpion 'move! distance) (set! scorpion-time 0))
                  ((eq? (scorpion 'kind) 'random-scorpion) (move-random! scorpion))))))
        (set! scorpion-time (+ scorpion-time delta-time))
        (if (> scorpion-time interval)
            (begin
              (for-each move! normal-scorpions)
              (for-each move! random-scorpions)))))

    (define (move-ant! direction)
      (if (and (can-move? ant direction 'wall) (can-move? ant direction 'door) (not (< ((next-position ant direction) 'y) top-border)))
          (begin
            (ant 'orientation! direction)
            (ant 'move! distance)
            (check-and-remove-eggs! direction)
            (check-for-key direction))))

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
        (else (error "[ERROR in LevelADT member?] Wrong object-list") (display object-list))))

    (define (length? object-list)
      (cond
        ((eq? object-list 'egg) (length eggs))
        ((eq? object-list 'key) (length keys))
        ((eq? object-list 'door) (length doors))
        (else (error "[ERROR in LevelADT length?] Wrong object-list") (display object-list))))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'add-walls) (apply add-walls parameters))
        ((eq? message 'walls) walls)
        ((eq? message 'add-scorpions) (apply add-scorpions parameters))
        ((eq? message 'normal-scorpions) normal-scorpions)
        ((eq? message 'random-scorpions) random-scorpions)
        ((eq? message 'add-eggs) (apply add-eggs parameters))
        ((eq? message 'eggs) eggs)
        ((eq? message 'add-puzzleobjects) (apply add-puzzleobjects parameters))
        ((eq? message 'keys) keys)
        ((eq? message 'inventory) inventory)
        ((eq? message 'doors) doors)
        ((eq? message 'add-powerups) (apply add-powerups parameters))
        ((eq? message 'power-ups) power-ups)
        ((eq? message 'initial-ant-pos) initial-ant-pos)
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
        ((eq? message 'end-point) end-point)
        ((eq? message 'done?) done?)
        (else  (error "[ERROR in LevelADT DISPATCH] Wrong message: ") (display message))))

    dispatch))