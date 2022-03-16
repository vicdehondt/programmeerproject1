(define (make-game . levels)
  (let* ((draw (make-draw))
        (current-level 1)
        (press-space-time 0)
        (shown? #t)
        (score 0)
        (lives 3)
        (highscore (read-file "highscore.txt"))
        (game-over? #f)
        (running #f))

    (define level (get-from-list current-level levels))

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

    (define (start-game!)
      (draw 'start! game-loop dispatch))

    ;; What happens every tick
    (define (game-loop delta-time)
      (if (not game-over?)
          (begin
            (draw 'update! dispatch)
            (update-score!)
            (update-lives!)
            (check-game-over)
            (level 'move-scorpion! delta-time)
            (level 'check-for-ant-scorpion-collision))))

    ;; What to do when a key is pressed
    (define (key-callback status key)
      (if (eq? status 'pressed)
          (begin
            (level 'move-ant! key)
            (start-game? key))))

    (define (update-score!)
      (if (level 'update-score?)
          (begin
            (level 'update-score! #f)
            (set! score (+ score 500)))))

    (define (update-lives!)
      (if (level 'remove-live?)
          (begin
            ;(reset-level!)
            (level 'remove-live! #f)
            (set! lives (- lives 1)))))

    (define (start-game? key)
      (if (not running)
          (if (eq? key #\space)
              (begin
                (start-game!)
                (set! running #t)))))

    #|(define (reset-level!)
      (display "Hallo")
      (set! level (get-from-list current-level levels))
      (start-game!))|#

    #|(define (check-level-done)
      (if ))|#

    (define (check-game-over)
      (if (<= lives 0)
          (begin
            (set! game-over? #t)
            (draw 'game-over!))))

    (define (dispatch message . parameters)
      (cond
        ((eq? message 'start!) (start-up!))
        ((eq? message 'level) level)
        ((eq? message 'score) score)
        ((eq? message 'highscore) highscore)
        ((eq? message 'lives) lives)
        (else  (error "[ERROR in GameADT DISPATCH] Wrong message: ") (display message))))

    dispatch))
