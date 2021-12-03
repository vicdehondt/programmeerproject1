(#%require (only racket random))
(#%require "Graphics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Globale Values en Procedures                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "constanten.rkt")
(load "hulp-procedures.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   ADT's                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tips bij het schrijven van een ADT:
;;
;; - Zorg er altijd voor dat je interne datastructuren intern blijven. Geef
;;   *nooit* een interne datastructuur vrij aan de hand van de dispatch methode.
;;   Uiteraard zijn waardes wel toegestaan.
;;
;; - Procedures die specifiek geschreven zijn voor de interne werking van een
;;   ADT mag je ook binnen het ADT plaatsen. Dit zorgt er voor dat niemand
;;   anders deze kan gebruiken. Tenzij natuurlijk dat zij ook van toepassing
;;   zijn op een ander ADT. Dan is het beter dat je ze publiek toegankelijk
;;   maakt (bv. globale scope).
;;
;; - Groepeer je procedures een beetje. Dit zorgt voor mooiere code. Zoals je
;;   hieronder zal zien heb ik een lijst met interne variabelen, dan een lijst
;;   met interne functies en vervolgens de dispatch functie.
;;
;; - Bij de implementatie van je ADTs kan het een goed zijn om een gelijkaardige
;;   implementatiestijl te gebruiken. Maak bijvoorbeeld gebruik van onderstaande
;;   structuur...
;;
;;     (define (maak-adt-objectje constructie-parameters ...)
;;       (let ((veldje-1 ...)
;;             (veldje-2 ...))
;;
;;         ;; Hulpprocedures
;;
;;         (define (bereken-iets) ...)
;;
;;         ;; Publieke procedures
;;
;;         (define (do-iets) ...)
;;
;;         ;; Dispatcher
;;
;;         (define (dispatcher-objectje msg)
;;           (cond ((eq? msg 'do-iets) do-iets)
;;                 ((eq? msg 'do-iets-anders) do-iets-anders)))
;;
;;         ;; Dispatcher teruggeven
;;         dispatcher-objectje))
;;
;;   Je kan ADT's ook op andere manieren implementeren. Maar deze worden niet
;;   getoond in dit WPO en moet je niet gebruiken voor je project.

(load "adt-positie.rkt")
(load "adt-appel.rkt")
(load "adt-slang-stuk.rkt")
(load "adt-slang.rkt")
(load "adt-level.rkt")
(load "adt-teken.rkt")
(load "adt-spel.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Spel Opstarten                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spel (maak-adt-spel))
((spel 'start))
