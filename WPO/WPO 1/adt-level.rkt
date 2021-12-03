;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Level ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dit voorbeeldspel bestaat uit slechts 1 level. We zouden de appel en slang
;; rechtstreeks in het Spel ADT kunnen geïmplementeerd hebben. Maar, als we
;; later echter zouden beslissen om een nieuw soort level toe te voegen dan moet
;; niet heel het Spel ADT aangepast worden. Door deze opsplitsing te maken moet
;; alleen de implementatie van het Level ADT aangepast worden.

;; maak-adt-level :: number, number -> level
(define (maak-adt-level aantal-cellen-breedte aantal-cellen-hoogte)
  (let* ((slang-start-positie
           (maak-adt-positie (quotient aantal-cellen-breedte 2)
                             (quotient aantal-cellen-hoogte 2)))
         (slang-adt (maak-adt-slang slang-start-positie))
         (appel-adt #f)
         (appel-tijd 0)
         (slang-tijd 0))
    
    ;;
    ;; Hulpprocedures
    ;;
    
    ;; Deze procedures genereren een random positie in de spelwereld.
    ;; In dit eenvoudig spel is er geen check om te controleren of er al een
    ;; object op de gegenereerde locatie is.
    
    ;; random-x-waarde :: / -> number
    (define (random-x-waarde)
      (random aantal-cellen-breedte))
    
    ;; random-y-waarde :: / -> number
    (define (random-y-waarde)
      (random aantal-cellen-hoogte))
    
    
    ;;
    ;; Logica Appel
    ;;
    
    ;; Deze is hier geïmplementeerd omdat voor een appel op een nieuwe positie
    ;; te zetten, de afmetingen van het spelbord geweten moeten worden.
    ;; Als dit geïmplementeerd zou zijn in het Appel ADT, dan zou het Appel ADT
    ;; ook afhankelijk zijn van het Level ADT. Om deze afhankelijkheid te
    ;; vermijden is deze logica hier geïmplementeerd.
    
    ;; random-positie :: / -> positie
    (define (random-positie)
      (let ((x (random-x-waarde))
            (y (random-y-waarde)))
        (maak-adt-positie x y)))
    
    ;; randomise-appel! :: / -> /
    (define (randomise-appel!)
      (if appel-adt
          (let ((nieuwe-positie (random-positie))
                (appel-positie (appel-adt 'positie)))
            ;; Verplaats de appel naar een nieuwe positie!
            ((appel-positie 'x!) (nieuwe-positie 'x))
            ((appel-positie 'y!) (nieuwe-positie 'y))
            ;; Reset de timer
            (set! appel-tijd 0))))
    
    ;; nieuwe-appel! :: / -> /
    (define (nieuwe-appel!)
      (set! appel-adt (maak-adt-appel (random-positie)))
      (set! appel-tijd 0))
    
    ;; beweeg-appel! :: / -> /
    (define (beweeg-appel! delta-tijd)
      (set! appel-tijd (+ appel-tijd delta-tijd))
      (if (> appel-tijd appel-refresh-rate)
          (randomise-appel!)))
    
    
    ;;
    ;; Logica Slang
    ;;
    
    ;; We hebben ervoor gekozen om alle logica dat te maken heeft met het
    ;; bewegen van de slang in het Level ADT zelf te implementeren. Dit omdat
    ;; een deel van de logica voor het bewegen afhankelijk is van de positie
    ;; van de appel. We zouden er ook voor gekozen kunnen hebben om deze in het
    ;; Slang ADT zelf te implementeren, maar dan moet het Slang ADT toegang
    ;; krijgen tot informatie dat bij het level hoort.
    ;; Het voordeel van dit hier te implementeren is dat we geen complexiteit
    ;; toevoegen om die data te delen. Het nadeel is dat een deel van de logica
    ;; die conceptueel bij het Slang ADT zou moeten horen, niet in het Slang ADT
    ;; geïmplementeerd is.
    ;; Bepaal in je eigen project wanneer je welke methode toepast!
    
    ;; beweeg-slang! :: / -> /
    (define (beweeg-slang!)
      (if (> slang-tijd slang-snelheid)
          (begin
            ;; Laat de slang 1 eenheid "vooruit" bewegen
            (slang-adt 'beweeg!)
            
            ;; Kijk of de slang botst met de appel.
            (if appel-adt
                (let* ((appel-positie (appel-adt 'positie))
                       (overlappingen ((slang-adt 'voor-alle-stukken)
                                       (lambda (stuk-adt)
                                         ((appel-positie 'vergelijk?)
                                          (stuk-adt 'positie))))))
                  (if (member #t overlappingen)
                      (begin (slang-adt 'verleng!)
                             (nieuwe-appel!)))))
            ;; Reset de timer.
            (set! slang-tijd 0))))
    
    ;; draai-slang! :: symbol -> /
    (define (draai-slang! toets)
      (cond
        ((eq? toets 'right)
         ((slang-adt 'richting!) 'rechts))
        ((eq? toets 'left)
         ((slang-adt 'richting!) 'links))
        ((eq? toets 'up)
         ((slang-adt 'richting!) 'omhoog))
        ((eq? toets 'down)
         ((slang-adt 'richting!) 'omlaag))))
    
    
    ;;
    ;; Algemene Logica
    ;;
    
    ;; update! :: number -> /
    (define (update! delta-tijd)
      (set! slang-tijd (+ slang-tijd delta-tijd))
      (beweeg-appel! delta-tijd)
      (beweeg-slang!))
    
    ;; toets :: any -> /
    (define (toets! toets)
      (draai-slang! toets))
    
    
    ;;
    ;; Initialisatie
    ;;
    
    ;; De appel moet onmiddellijk op een willekeurige positie starten...
    
   
    ;; OPGAVE: (7/7) Maak een value van het Appel ADT, en sla deze op in de
    ;; `appel-adt` variabele. Maak gebruik van de procedure `random-positie` om
    ;; een willekeurige positie te genereren.
   
   (nieuwe-appel!)
    
    ;;
    ;; Dispatch
    ;;
    
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'toets!) toets!)
            ((eq? msg 'appel) appel-adt)
            ((eq? msg 'slang) slang-adt)))
    dispatch-level))
