(#%require "Graphics.rkt")

(define (make-draw)
  (let ((window (make-window 1280 800 "Fire Ant")))

    (define base-layer (window ('make-layer)))

    (define movingobjects-layer (window ('make-layer)))
    
    (define (start!)
      ((window 'set-background!) "black"))
    
    #|
    (define (teken-object! obj tile)
      (let* ((position (obj 'positie))
            (new-x (* (position 'x) cel-breedte-px))
            (new-y (* (position 'y) cel-hoogte-px)))
        ((tile 'set-x!) new-x)
        ((tile 'set-y!) new-y)))

    (define (set-spel-lus-functie! fun)
      ((venster 'set-update-callback!) fun))
    
    (define (set-toets-functie! fun)
      ((venster 'set-key-callback!) fun))
    |#
    (define (dispatch msg)
      (cond ((eq? msg 'start!) (start!))
            ((eq? msg 'update!) (display "test2"))))
    dispatch))