;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Slang ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maak-adt-slang start-positie)
  (let ((stukken (list (maak-adt-slang-stuk start-positie)))
        (richting 'omhoog))
    
    ;; Deze procedure voegt een stuk toe aan de slang. Namelijk aan de
    ;; voorkant van de slang. Dit doen we om twee redenen. Enerzijds omdat
    ;; `cons` de snelste manier is om dit te doen, maar anderzijds ook omdat
    ;; dit het updaten van de slang gemakkelijker maakt. Alle andere stukken
    ;; mogen blijven staan. Dit zal duidelijk worden bij het updaten van de
    ;; positie.
    
    ;; maak-langer! :: / -> /
    (define (maak-langer!)
      (let* ((hoofd (car stukken))
             (nieuwe-positie (((hoofd 'positie) 'beweeg) richting))
             (nieuw-stuk (maak-adt-slang-stuk nieuwe-positie)))
        (set! stukken (cons nieuw-stuk stukken))))
    
    ;; Deze procedure laat de slang bewegen. Dit doet hij door de positie van
    ;; het hoofd-object te nemen, en aan de hand van de huidige richting een
    ;; nieuwe positie te berekenen. Daarna wordt de positie van elk deeltje één
    ;; plaats opgeschoven zodanig dat het eerste deeltje op de nieuwe positie
    ;; staat, en het tweede deeltje op de oude positie van het eerste deeltje
    ;; en zo verder...
    
    ;; beweeg! :: / -> /
    (define (beweeg!)
      (define (iter lst new-pos)
        (let* ((first (car lst))
               (rest (cdr lst))
               (old-pos (first 'positie)))
          ((first 'positie!) new-pos)
          (if (not (null? rest))
              (iter rest old-pos))))
      (let* ((hoofd (car stukken))
             (volgende-positie (((hoofd 'positie) 'beweeg) richting)))
        (iter stukken volgende-positie)))
    
    ;; set-richting! :: symbol -> /
    (define (set-richting! r)
      (set! richting r))
    
    ;; voor-alle-stukken :: (slang-stuk -> any) -> list
    (define (voor-alle-stukken f)
      (map f stukken))
    
   
    
    (define (dispatch-slang msg)
      (cond ((eq? msg 'verleng!) (maak-langer!))
            ((eq? msg 'richting!) set-richting!)
            ((eq? msg 'beweeg!) (beweeg!))
            ((eq? msg 'voor-alle-stukken) voor-alle-stukken)))
    dispatch-slang))
