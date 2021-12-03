;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maak-adt-spel :: / -> spel
(define (maak-adt-spel)
  ;; Dit is het eigenlijk spel object.
  (let ((level-adt (maak-adt-level spel-breedte spel-hoogte))
        (teken-adt (maak-adt-teken venster-breedte-px venster-hoogte-px)))
    
;; toets-procedure :: symbol, any -> /
(define (toets-procedure status toets)
  ;; status is ofwel gelijk aan ...
  ;; - 'pressed: wanneer de toets ingedrukt wordt
  ;; - 'released: wanneer de toets losgelaten wordt
  ;; Wanneer de toets voor lange tijd ingedrukt wordt, dan wordt deze
  ;; procedure meermaals aangeroepen waarbij status gelijk is aan 'pressed
  ;; voor dezelfde toets!
  (if (eq? status 'pressed)
    ((level-adt 'toets!) toets)))

;; spel-lus-procedure :: number -> /
(define (spel-lus-procedure delta-tijd)
  ((level-adt 'update!) delta-tijd)
  ((teken-adt 'teken-spel!) dispatch-spel))

;; Functie voor het starten van het spel.
;; start :: / -> /
(define (start)
  ;; Zet de callbacks via het teken ADT
  ((teken-adt 'set-spel-lus-functie!) spel-lus-procedure)
  ((teken-adt 'set-toets-functie!) toets-procedure))
    
    ;; Dispatch functie
    (define (dispatch-spel msg)
      (cond ((eq? msg 'start) start)
            ((eq? msg 'level) level-adt)))
    
    dispatch-spel))
