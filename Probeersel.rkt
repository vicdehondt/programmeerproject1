; Dit is een test bestand

(#%require "Graphics.rkt")

(#%require (only racket random error))

(define venster (make-window 600 400 "Test venster"))
((venster 'set-background!) "blue")

(define mijn-eerste-laag (venster 'make-layer))

(define mijn-tile (make-tile 200 100))

((mijn-tile 'draw-rectangle) 10 10 180 80 "red")
((mijn-tile 'draw-text) "Rini Stinki" 24 12 50 "white")

;(define mijn-tile (make-tile 50 100))
;((mijn-tile 'draw-rectangle) 20 20 50 100 "red")

((mijn-eerste-laag 'add-drawable) mijn-tile)
