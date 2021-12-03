;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Constanten                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We maken eerst een lijstje met variabelen die ons spel zullen
;; configureren. Uiteraard willen we niet dat deze "magic constants" doorheen
;; het hele programma verspreid staan. Dit heeft als gevolg dat wanneer je
;; bijvoorbeeld het formaat van het spel wil aanpassen je dan doorheen je hele
;; code moet graven en zoeken om dit te doen. Het abstraheren van deze waardes
;; in variabelen zorgt voor heel flexibele code.

(define cel-breedte-px 20)
(define cel-hoogte-px 20)

(define spel-breedte 20)
(define spel-hoogte 20)

(define venster-breedte-px (* cel-breedte-px spel-breedte))
(define venster-hoogte-px (* cel-hoogte-px spel-hoogte))

;; Hoe lang een appel op dezelfde plaats blijft staan...
(define appel-refresh-rate 20000) ;; 20000 milliseconden = 20 seconden

;; Aan welke snelheid de slang ongeveer beweegt...
(define slang-snelheid 200) ;; 200 milliseconden = 0.2 seconden

;; De code in dit bestand moet abstractie maken van hoe of wat de elementen
;; (de appel en de slang) getekend worden. Deze taken zijn namelijk uitbesteed
;; aan het teken-ADT (en zo aan de Graphics.rkt library). Vermits de code hier
;; niet weet of het teken-ADT gaat tekenen naar een computerscherm, of elke
;; frame naar een kleurenprinter gaat sturen, of gaat uitbeelden in kiezeltjes
;; op de grond, of... mag de code in dit bestand niet geschreven worden in
;; functie van pixels (of punten/kiezels/etc.) In plaats daarvan werken we met
;; een abstract grid dat het speelvenster voorstelt. De volgende berekeningen
;; zijn nodig voor het Teken ADT. Het is jouw taak om de spellogica zodanig te
;; schrijven dat deze onafhankelijk is van de tekenlogica.


