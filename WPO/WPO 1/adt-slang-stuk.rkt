;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Slang Stuk ADT                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We willen een slang voorstellen. Dit doen we natuurlijk door meerdere
;; lichaamsdelen te tekenen. We hebben een hoofd en de rest van de staart die
;; bestaat uit verschillende blokjes. Om het gemakkelijk te maken stellen we het
;; hoofd hetzelfde voor als het lichaam. Dit wil zeggen dat de slang
;; uiteindelijk zal bestaan uit een lijst van objectjes van het Slang Stuk ADT.

;; maak-adt-slang-stuk :: positie -> slang-stuk
(define (maak-adt-slang-stuk positie)
  
  ;; positie! :: positie -> /
  (define (positie! nieuwe-positie)
    (set! positie nieuwe-positie))
  
  ;; Dispatch functie
  (define (dispatch-slang-stuk msg)
    (cond ((eq? msg 'positie) positie)
          ((eq? msg 'positie!) positie!)))
  
  dispatch-slang-stuk)
