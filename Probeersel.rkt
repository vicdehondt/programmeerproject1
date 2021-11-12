; Dit is een test bestand

(#%require "Graphics.rkt")

(#%require (only racket random error))

(define venster (make-window 1920 1080 "Test venster"))
((venster 'set-background!) "blue")

(define mijn-tile (make-tile 200 100))

((mijn-tile 'draw-rectangle) 10 10 180 80 "red")
((mijn-tile 'draw-text) "Rini Stinki" 24 200 50 "white")