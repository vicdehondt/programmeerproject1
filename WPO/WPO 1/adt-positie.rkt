;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Positie ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maak-adt-positie :: number, number -> position
(define (maak-adt-positie x y)
  (let ((x-value x)
        (y-value y))
 
  
 
    ;; OPGAVE: (1/7) Implementeer twee *destructieve* operaties om de x- en
    ;; y-coördinaat van een positie aan te passen.

    (define (x! new-x)
      (set! x-value new-x))

    (define (y! new-y)
      (set! y-value new-y))
 
    ;; OPGAVE: (2/7) Implementeer een predicaat-procedure die x- en y-waardes van
    ;; twee posities vergelijkt.

    (define (vergelijk? second-object)
      (and (eq? x-value (second-object 'x)) (eq? y-value (second-object 'y))))
 
 
    ;; OPGAVE: (3/7) Implementeer een functie die een nieuw positie object
    ;; teruggeeft die 1 eenheid op de x- of y-as opschuift aan de hand van een
    ;; meegegeven richting. Gebruik de symbolen: 'omhoog, 'omlaag, 'links, en
    ;; 'rechts.

    (define (beweeg towards)
      (cond
        ((eq? towards 'omhoog) (maak-adt-positie x-value (- y-value 1)))
        ((eq? towards 'omlaag) (maak-adt-positie x-value (+ y-value 1)))
        ((eq? towards 'links) (maak-adt-positie (- x-value 1) y-value))
        ((eq? towards 'rechts) (maak-adt-positie (+ x-value 1) y-value))))
    
    (define (dispatch-positie msg)
      (cond ((eq? msg 'x) x-value)
            ((eq? msg 'y) y-value)
            ((eq? msg 'x!) x!)
            ((eq? msg 'y!) y!)
            ((eq? msg 'beweeg) beweeg)
            ((eq? msg 'vergelijk?) vergelijk?)))
    dispatch-positie))

;; Gebruik onderstaande code in de Read-Eval-Print Loop na alleen dit bestand
;; uitgevoerd te hebben, voor je implementatie te testen.
;; Wanneer je alle oefeningen geïmplementeerd hebt kan je beginnen met de
;; andere oefeningen.

;; Merk op, naast ; kan je in Scheme ook commentaar in je code plaatsen tussen
;; een #| en een |#: deze commentaar kan, in tegenstelling tot ;, verspreid
;; zijn over meerdere lijnen.

;; Oefening 1:

#|
(define p1 (maak-adt-positie 100 200))
(display (p1 'x)) (newline) ;; Print 100 in de REPL
(display (p1 'y)) (newline) ;; Print 200 in de REPL
((p1 'x!) 0)
(display (p1 'x)) (newline) ;; Print 0 in de REPL: want we hebben in de vorige lijn code de x-coördinaat aangepast naar 0
|#


;; Oefening 2:

#|
(define p1 (maak-adt-positie 100 200))
(define p2 (maak-adt-positie 100 600))
(display ((p1 'vergelijk?) p2)) (newline) ;; Print #f in de REPL: beide posities hebben niet dezelfde x- en y-coördinaat.
((p2 'y!) 200)
(display ((p1 'vergelijk?) p2)) (newline) ;; Print #t in de REPL: beide posities hebben nu wel dezelfde x- en y-coördinaat.

;; Merk op:
(display (eq? p1 p2)) (newline) ;; Print #f in de REPL: beide posities zijn afzonderlijke "objecten".
;; Onze eigen vergelijk-procedure vergelijkt de inhoud van 2 objecten. Scheme kijkt (met eq?) of dat ze hetzelfde object zijn.
;; Scheme heeft geen idee hoe "objecten" (i.e. dispatch-procedures) vergeleken kunnen worden buiten te kijken of het dezelfde
;; procedure is met dezelfde lexicale omgeving.
|#


;; Oefening 3:
#|
(define p1 (maak-adt-positie 0 0))
(define p2 ((p1 'beweeg) 'rechts))
(display (p1 'x)) (newline) ;; Print 0 in de REPL: de originele x-coördinaat van p1 is niet aangepast.
(display (p2 'x)) (newline) ;; Print 1 in de REPL: p2 is een nieuw objectje met zijn eigen x- en y-coördinaat.
;; Merk op: het originele objectje (p1) werd niet aangepast door 'beweeg te sturen (naar p1)...
(display (p1 'x)) (newline) ;; Print 0 in de REPL
(display (p1 'y)) (newline) ;; Print 0 in de REPL
|#
