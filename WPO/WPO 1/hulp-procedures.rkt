;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Hulp Procedures                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Het schrijven van hulp functies is altijd een goede gewoonte. Soms moet je
;; operaties doen op waarden die bijna generiek zijn. Bijvoorbeeld, je wil voor
;; een bepaalde lijst een totaal berkenen. Dit kan een lijst van objecten zijn,
;; getallen etc. In plaats van dan een functie te schrijven die specifiek doet
;; wat jij wil, schrijf je een iets generiekere functie. Deze zet je dan apart
;; en kan je in de toekomst nog van dienst zijn.

;; debugging? :: boolean
(define debugging? #t)

;; debug :: any -> /
(define (debug . msg)
  (if debugging?
      (begin (display msg)
             (newline))))
