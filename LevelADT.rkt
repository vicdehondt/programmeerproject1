(define (make-level level-number initial-ant-pos top-border bottom-border end-point)
  (let ((scorpion-time 0)
        (shield-time 0)
        (speed-up-time 0)
        (ant (make-movingobject initial-ant-pos 'right 'ant))
        (inventory '((empty) (empty) (empty)))
        (update-score? #f)
        (lives 0)
        (remove-live? #f)
        (add-live? #f)
        (shield? #f)
        (speed-up? #f)
        (walls '())
        (normal-scorpions '())
        (random-scorpions '())
        (eggs '())
        (keys '())
        (doors '())
        (shield-shrooms '())
        (food '())
        (reset-level? #f)
        (done? #f)
        (back-up (make-vector 5 0)))


    (define (duplicate!)
      (let ((new-level (make-level level-number initial-ant-pos top-border bottom-border end-point)))
        (new-level 'add-walls (vector-ref back-up wall-place))
        (new-level 'add-eggs (vector-ref back-up egg-place))
        (new-level 'add-scorpions (vector-ref back-up scorpion-place))
        (new-level 'add-puzzleobjects (vector-ref back-up puzzle-object-place))
        (new-level 'add-powerups (vector-ref back-up power-up-place))
        new-level))

    
    (define (initial-ant-pos! position-object)
      (set! initial-ant-pos position-object))

    (define (check-deactivate-speed-up! delta-time)
      (if (> speed-up-time speed-up-interval)
          (begin
            (set! speed-up? #f)
            (set! speed-up-time 0))))

    (define (speed-up delta-time)
      (set! speed-up-time (+ speed-up-time delta-time))
      (if (and (member (random 1 128) '(5 53 27 83 63 101)) (> speed-up-time 15000))
          (begin
            (set! speed-up? #t)
            (set! speed-up-time 0))))

    ;;
    ;; ADD PROCEDURES
    ;;

    ;; Walls

    (define (add-walls lst)
      (vector-set! back-up wall-place lst)
      (for-each (lambda (positions) (add-multiple-walls (make-position (caar positions) (cadar positions))  (make-position (caadr positions) (cadadr positions)))) lst))

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
    
    (define (add-wall position-object)
      (set! walls (cons (make-wall (make-position (position-object 'x) (position-object 'y))) walls)))

    
    ;; Scorpions

    (define (add-scorpions lst)
      (vector-set! back-up scorpion-place lst)
      (for-each (lambda (scorpion-info) (add-scorpion (make-position (list-ref scorpion-info 0)
                                                                     (list-ref scorpion-info 1))
                                                      (list-ref scorpion-info 2)
                                                      (list-ref scorpion-info 3))) lst))

    (define (add-scorpion position-object orientation kind)
      (cond
        ((eq? kind 'normal) (set! normal-scorpions (cons (make-movingobject position-object orientation 'normal-scorpion) normal-scorpions)))
        ((eq? kind 'random) (set! random-scorpions (cons (make-movingobject position-object orientation 'random-scorpion) random-scorpions)))
        (else (error "[ERROR in LevelADT add-scorpion] Wrong kind!"))))


    ;; Eggs

    (define (add-eggs lst)
      (vector-set! back-up egg-place lst)
      (for-each (lambda (x-and-y) (add-egg (make-position (car x-and-y) (cadr x-and-y)))) lst))
    
    (define (add-egg position-object)
      (set! eggs (cons (make-egg position-object) eggs)))


    ;; Puzzle-objects

    (define (add-puzzleobjects lst)
      (vector-set! back-up puzzle-object-place lst)
      (for-each (lambda (x-y-kind) (add-puzzleobject (make-position (car x-y-kind) (cadr x-y-kind)) (caddr x-y-kind))) lst))

    (define (add-puzzleobject position-object kind)
      (cond
        ((eq? kind 'key) (set! keys (cons (make-puzzleobject position-object kind) keys)))
        ((eq? kind 'door) (set! doors (cons (make-puzzleobject position-object kind) doors)))
        (else (error "[ERROR in LevelADT add-puzzleobject] Wrong kind!"))))


    ;; Power-ups

    (define (add-powerups lst)
      (vector-set! back-up power-up-place lst)
      (for-each (lambda (x-y-kind) (add-powerup (make-position (car x-y-kind) (cadr x-y-kind)) (caddr x-y-kind))) lst))

    (define (add-powerup position-object kind)
      (cond
        ((eq? kind 'shield) (set! shield-shrooms (cons (make-powerup position-object 'shield-shroom) shield-shrooms)))
        ((eq? kind 'food) (set! food (cons (make-powerup position-object 'food) food)))
        (else (error "[ERROR in LevelADT add-powerup] Wrong kind!"))))

    ;;
    ;; COLLISION DETECTION
    ;;

    ;; Can a given object move in the given direction without obstruction? (main collision-detection procedure)
    (define (can-move? moving-object direction kind)

      (define (check-for-key)
        (cond
          ((and (list? (member #t (door-collision-list moving-object direction))) (eq? (caar inventory) 'key)) (remove! 'door direction) #t)
          ((and (list? (member #t (door-collision-list moving-object direction))) (eq? (caar inventory) 'empty)) #f)
          (else #t)))

      (cond
        ((eq? kind 'wall) (not (list? (member #t (wall-collision-list moving-object direction)))))
        ((eq? kind 'door) (if (eq? (moving-object 'kind) 'ant) (check-for-key)
                              (not (list? (member #t (door-collision-list moving-object direction))))))
        ((eq? kind 'egg) (not (list? (member #t (egg-collision-list moving-object)))))
        ((eq? kind 'key) (not (list? (member #t (key-collision-list moving-object)))))
        ((eq? kind 'shield-shroom) (not (list? (member #t (shield-shroom-collision-list moving-object)))))
        ((eq? kind 'food) (not (list? (member #t (food-collision-list moving-object)))))
        (else (error "[ERROR in LevelADT can-move?] Wrong kind!"))))


    ;; Collision-lists: gives list back with #t (collision) and #f (no obstruction)
    
    (define (egg-collision-list moving-object)
      (map (lambda (egg-object) (collision? moving-object egg-object)) eggs))

    (define (shield-shroom-collision-list moving-object)
      (map (lambda (shield-object) (collision? moving-object shield-object)) shield-shrooms))
    
    (define (food-collision-list moving-object)
      (map (lambda (food-object) (collision? moving-object food-object)) food))

    (define (wall-collision-list moving-object direction)
      (map (lambda (wall-object) (upcomming-collision? moving-object (wall-object 'position) direction)) walls))

    (define (door-collision-list moving-object direction)
      (map (lambda (door-object) (upcomming-collision? moving-object (door-object 'position) direction)) doors))

    (define (key-collision-list moving-object)
      (map (lambda (key-object) (collision? moving-object key-object)) keys))


    ;; Collision checkers
    
    (define (collision? moving-object object)
      ((object 'position) 'equal? (moving-object 'position)))

    (define (upcomming-collision? moving-object position direction)
      (position 'equal? (next-position moving-object direction)))

    ;; Gives moving-object's next position based on the direction given
    (define (next-position moving-object direction)
      (let ((current-x ((moving-object 'position) 'x))
            (current-y ((moving-object 'position) 'y)))
        (cond
          ((eq? direction 'right) (make-position (+ current-x 1) current-y))
          ((eq? direction 'left) (make-position (- current-x 1) current-y))
          ((eq? direction 'up) (make-position current-x (- current-y 1)))
          ((eq? direction 'down) (make-position current-x (+ current-y 1)))
          (else (error "[ERROR in LevelADT next-position] direction!")))))

    ;;
    ;; REMOVAL
    ;;

    (define (remove! kind . direction)
      (define (search-and-remove! lst compare-func)
        (cond
          ((null? lst) '())
          ((and (null? (cdr lst)) (((car lst) 'position) 'equal? compare-func)) '())
          ((((car lst) 'position) 'equal? compare-func) (cdr lst))
          (else (cons (car lst) (search-and-remove! (cdr lst) compare-func)))))

      (cond
        ((eq? kind 'door) (remove-from-inventory! 'key) (set! doors (search-and-remove! doors (next-position ant (car direction)))))
        ((eq? kind 'egg) (set! eggs (search-and-remove! eggs (ant 'position))))
        ((eq? kind 'shield-shroom) (set! shield-shrooms (search-and-remove! shield-shrooms (ant 'position))))
        ((eq? kind 'food) (set! food (search-and-remove! food (ant 'position))))
        (else (error "[ERROR in LevelADT remove!] Wrong kind!"))))

    (define (remove-from-inventory! kind)
      (let ((keys (car inventory)))
        (cond
          ((eq? kind 'key) (set-car! inventory (cdr keys)))
          (else (error "[ERROR in LevelADT remove-from-inventory!]")))))

    (define (remove-and-add-to-inv! kind)
      (define (look-for-remove lst)
        (cond
          ((null? lst) '())
          ((and (null? (cdr lst)) (((car lst) 'position) 'equal? (ant 'position))) (add-to-inventory! (car lst)) '())
          ((((car lst) 'position) 'equal? (ant 'position)) (add-to-inventory! (car lst)) (cdr lst))
          (else (cons (car lst) (look-for-remove (cdr lst))))))
      (cond
        ((eq? kind 'key) (set! keys (look-for-remove keys)))))

    (define (add-to-inventory! object)
      (cond
        ((eq? (object 'kind) 'key) (set-car! inventory (cons 'key (car inventory))))
        (else (error "[ERROR in LevelADT add-to-inventory!] Wrong kind!"))))

    ;;
    ;; LIVES
    ;;

    (define (lives! value)
      (set! lives value))

    ;;
    ;; CHECKERS
    ;;

    ;; Checks if something needs to be removed
    (define (check-and-remove! direction)
      (cond
        ((not (can-move? ant direction 'egg)) (remove! 'egg) (update-score! (cons 500 (vector 0 0 0 0 0 5 0 0))))
        ((not (can-move? ant direction 'key)) (remove-and-add-to-inv! 'key))
        ((not (can-move? ant direction 'shield-shroom)) (remove! 'shield-shroom) (set! shield? #t))
        ((not (can-move? ant direction 'food)) (remove! 'food) (lives! (+ lives 1)) (update-score! (cons 200 (vector 0 0 0 0 0 2 0 0))))))

    ;; Checks if any scorpion collides with the ant and moves the ant back to initial-ant-pos
    (define (check-for-ant-scorpion-collision)
      (define (ant-scorpion-collision? lst)
        (list? (member #t (map (lambda (scorpion-object) ((ant 'position) 'equal? (scorpion-object 'position))) lst))))
      (if (and (not shield?) (or (ant-scorpion-collision? normal-scorpions)
                                 (ant-scorpion-collision? random-scorpions)))
          (begin
            (lives! (- lives 1))
            (if (> lives 1) (set! reset-level? #t)))))

    (define (check-deactivate-shield! delta-time)
      (set! shield-time (+ shield-time delta-time))
      (if (> shield-time shield-interval)
          (begin
            (set! shield? #f)
            (set! shield-time 0))))

    ;;
    ;; RESET
    ;;

    (define (place-back! lst)
      (define (search-and-place! object)
        (cond
          ((eq? (object 'kind) 'eggs) (set! eggs (cons object eggs)))
          ((eq? (object 'kind) 'keys) (set! keys (cons object keyss)))
          ((eq? (object 'kind) 'doors) (set! doors (cons object doors)))
          ((eq? (object 'kind) 'shield-shrooms) (set! shield-shrooms (cons object shield-shrooms)))
          ((eq? (object 'kind) 'food) (set! food (cons object food)))))
      (for-each search-and-place! lst))

    (define (reset-level! value)
      (set! reset-level? value))
    
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
                  ((eq? (scorpion 'kind) 'random-scorpion) (move-random! scorpion))
                  (else (error "[ERROR in LevelADT move!] Wrong kind!"))))))
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
            (check-and-remove! direction))))

    ;;
    ;; SCORE
    ;;

    (define (update-score! value)
      (set! update-score? value))
    
    ;;
    ;; DATA ABSTRACTION PROCEDURES
    ;;
    
    (define (member? element object-list)
      (cond
        ((eq? object-list 'egg) (member element eggs))
        ((eq? object-list 'key) (member element keys))
        ((eq? object-list 'door) (member element doors))
        ((eq? object-list 'shield-shroom) (member element shield-shrooms))
        ((eq? object-list 'food) (member element food))
        (else (error "[ERROR in LevelADT member?] Wrong object-list!"))))

    (define (length? object-list)
      (cond
        ((eq? object-list 'egg) (length eggs))
        ((eq? object-list 'key) (length keys))
        ((eq? object-list 'door) (length doors))
        ((eq? object-list 'shield-shroom) (length shield-shrooms))
        ((eq? object-list 'food) (length food))
        (else (error "[ERROR in LevelADT length?] Wrong object-list!"))))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'duplicate) (apply duplicate! parameters))
        ((eq? message 'initial-ant-pos) initial-ant-pos)
        ((eq? message 'end-point) end-point)
        ((eq? message 'ant) ant)
        ((eq? message 'inventory) inventory)
        ((eq? message 'update-score?) update-score?)
        ((eq? message 'remove-live?) remove-live?)
        ((eq? message 'speed-up?) speed-up?)
        ((eq? message 'shield?) shield?)
        ((eq? message 'walls) walls)
        ((eq? message 'normal-scorpions) normal-scorpions)
        ((eq? message 'random-scorpions) random-scorpions)
        ((eq? message 'eggs) eggs)
        ((eq? message 'keys) keys)
        ((eq? message 'doors) doors)
        ((eq? message 'shield-shrooms) shield-shrooms)
        ((eq? message 'food) food)
        ((eq? message 'done?) done?)
        ((eq? message 'initial-ant-pos!) (apply initial-ant-pos! parameters))
        ((eq? message 'add-walls) (apply add-walls parameters))
        ((eq? message 'add-scorpions) (apply add-scorpions parameters))
        ((eq? message 'add-eggs) (apply add-eggs parameters))
        ((eq? message 'add-puzzleobjects) (apply add-puzzleobjects parameters))
        ((eq? message 'add-powerups) (apply add-powerups parameters))
        ((eq? message 'lives) lives)
        ((eq? message 'lives!) (apply lives! parameters))
        ((eq? message 'remove-live!) (apply remove-live! parameters))
        ((eq? message 'speed-up) (apply speed-up parameters))
        ((eq? message 'check-deactivate-speed-up!) (apply check-deactivate-speed-up! parameters))
        ((eq? message 'check-for-ant-scorpion-collision) (check-for-ant-scorpion-collision))
        ((eq? message 'check-deactivate-shield!) (apply check-deactivate-shield! parameters))
        ((eq? message 'move-ant!) (apply move-ant! parameters))
        ((eq? message 'move-scorpion!) (apply move-scorpion! parameters))
        ((eq? message 'update-score!) (apply update-score! parameters))
        ((eq? message 'reset-level?) reset-level?)
        ((eq? message 'reset-level!) (apply reset-level! parameters))
        ((eq? message 'member?) (apply member? parameters))
        ((eq? message 'length?) (apply length? parameters))
        (else  (error "[ERROR in LevelADT DISPATCH] Wrong message!"))))

    dispatch))