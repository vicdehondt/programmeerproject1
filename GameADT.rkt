(define (make-game . levels)
  (let* ((draw (make-draw))
         (levels-vector (list-to-vector levels))
         (current-level 1)
         (back-up '())
         (press-space-time 0)
         (bomb-animation-time 0)
         (speed-up-time 0)
         (rumble-time 0)
         (shown? #t)
         (score 0)
         (score-vect (make-vector 8 0))
         (lives lives-start-count)
         (highscore (cadr (read-file "highscore.txt")))
         (highscore-vect (car (read-file "highscore.txt")))
         (game-over? #f)
         (running #f))

    (define (level) (vector-ref levels-vector (- current-level 1)))

    (define (start-up!)
      (draw 'show-splash! key-callback press-space!))

    (define (press-space! delta-time)
      (set! press-space-time (+ press-space-time delta-time))
      (if (> press-space-time press-space-interval)
          (if shown?
              (begin
                (set! shown? #f)
                (draw 'press-space shown?)
                (set! press-space-time 0))
              (begin
                (set! shown? #t)
                (draw 'press-space shown?)
                (set! press-space-time 0)))))

    (define (check-speed-up delta-time)
      (set! speed-up-time (+ speed-up-time delta-time))
      (if (and (member (random 1 128) '(5 53 27 83 63 101)) (> speed-up-time speed-up-interval))
          (begin
            ((level) 'speed-up #t)
            (draw 'speed-up #t)
            (set! speed-up-time 0))))

    (define (check-deactivate-speed-up! delta-time)
      (if (> speed-up-time speed-up-interval)
          (begin
            ((level) 'speed-up #f)
            (draw 'speed-up #f)
            (set! speed-up-time 0))))

    (define (start-game!)
      (draw 'start! game-loop dispatch)
      ((level) 'lives! lives-start-count))

    ;; What happens every tick
    (define (game-loop delta-time)
      (if (not game-over?)
          (begin
            (draw 'update! dispatch)
            (update-score!)
            (check-game-over)
            (next-level?)
            (reset-level?)
            (check-bomb-animation delta-time)
            ((level) 'move-scorpion! delta-time)
            ((level) 'check-for-ant-scorpion-collision)
            (if ((level) 'shield?) ((level) 'check-deactivate-shield! delta-time))
            (if ((level) 'speed-up) (check-deactivate-speed-up! delta-time) (check-speed-up delta-time)))))

    ;; What to do when a key is pressed
    (define (key-callback status key)
      (if (eq? status 'pressed)
          (begin
            (if (or (eq? key 'up) (eq? key 'down) (eq? key 'left) (eq? key 'right)) ((level) 'move-ant! key))
            (start-game? key))))

    (define (add vect1 vect2)
      (define byte (make-vector 8))
      (define carry 0)
      (do ((pos 7 (- pos 1))) ((< pos 0) byte)
        (let ((cnt (+ (vector-ref vect2 pos)
                      (vector-ref vect1 pos)
                      carry)))
          (vector-set! vect1 pos (if (>= cnt 10) (- cnt 10) cnt))
          (set! carry (if (>= cnt 10) 1 0)))))


    (define (update-score!)
      (let ((update? ((level) 'update-score?)))
        (if update?
            (begin
              ((level) 'update-score! #f)
              (set! score (+ score (car update?)))
              (add score-vect (cdr update?))
              (draw 'update-score! score-vect)
              (if (> score highscore)
                  (begin
                    (add highscore-vect (cdr update?))
                    (set! highscore (+ highscore (car update?)))
                    (draw 'update-highscore! highscore-vect)
                    (write-file "highscore.txt" (list highscore-vect highscore))))))))

    (define (check-bomb-animation delta-time)
      (if ((level) 'bomb-animation?)
          (begin
            (bomb-animation delta-time))))

    (define (bomb-animation delta-time)
      (draw 'bomb-animation! bomb-animation)
      (set! bomb-animation-time (+ bomb-animation-time delta-time))
      (if (> bomb-animation-time bomb-animation-interval)
          (begin
            ((level) 'bomb-animation? #f)
            (draw 'set-black!)
            (draw 'continue! game-loop)
            (set! bomb-animation-time 0))
          (begin
            (set! rumble-time (+ rumble-time delta-time))
            (if (> rumble-time rumble-interval)
                (begin
                  (draw 'set-opposite-background!)
                  (set! rumble-time 0))))))

    (define (check-deactivate-shield! delta-time)
      (set! bomb-animation-time (+ bomb-animation-time delta-time))
      (if (> bomb-animation-time bomb-animation-interval)
          (begin
            (set! shield? #f)
            (set! shield-time 0))))

    (define (next-level!)
      (let ((current-lives ((level) 'lives)))
        (set! current-level (+ current-level 1))
        ((level) 'lives! current-lives)
        (draw 'initialize! dispatch)))
    
    (define (next-level?)
      (if ((((level) 'ant) 'position) 'equal? ((level) 'end-point))
          (let ((max-levels (vector-length levels-vector)))
            (if (= current-level max-levels)
                (begin
                  (set-back-up-level-active!)
                  (set! current-level 1)
                  (set! running #f)
                  (draw 'game-win! press-space!))
                (begin
                  (set-back-up-level-active!)
                  (next-level!))))))

    (define (set-back-up-level-active!)
      (let ((back-up ((level) 'duplicate))
            (current-lives ((level) 'lives)))
        (set! lives current-lives)
        (back-up 'lives! lives)
        (vector-set! levels-vector (- current-level 1) back-up)))

    (define (reset-level?)
      (if ((level) 'reset-level?)
          (begin
            (set-back-up-level-active!)
            ((level) 'reset-level! #f)
            (draw 'initialize! dispatch)
            (display ((level) 'inventory)))))

    (define (start-game? key)
      (if (not running)
          (if (eq? key #\space)
              (begin
                (start-game!)
                (set! score 0)
                (set! score-vect (make-vector 8 0))
                (draw 'update-score! score-vect)
                (set! game-over? #f)
                (set! running #t)))))

    (define (check-game-over)
      (if (<= ((level) 'lives) 0)
          (begin
            (set-back-up-level-active!)
            (set! current-level 1)
            (set! game-over? #t)
            (set! running #f)
            (draw 'game-over! press-space!))))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'start!) (start-up!))
        ((eq? message 'level) (level))
        ((eq? message 'score) score-vect)
        ((eq? message 'current) current-level)
        ((eq? message 'highscore) highscore-vect)
        ((eq? message 'lives) ((level) 'lives))
        (else  (error "[ERROR in GameADT DISPATCH] Wrong message!"))))

    dispatch))
