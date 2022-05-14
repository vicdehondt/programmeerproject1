;; Executes a function on each element of the given abstract data-structure
(define (for-each-object function lst)
  (for-each function lst))

(define (when test consequent)
  (if test
      consequent))

(define (read-file filename)
      (let* ((read-file (open-input-file filename))
             (data (read read-file)))
        data))

(define (write-file filename value)
      (let ((port (open-output-file filename #:exists 'replace)))
    (display value port)
    (close-output-port port)))

(define (get-from-list place list)
  (let find ((count 1)
             (lst list))
    (cond
      ((null? lst) (display "Error in get-from-list"))
      ((= count place) (car lst))
      (else (find (+ count 1) (cdr lst))))))

(define (list-to-vector lst)
  (let loop ((vect (make-vector (length lst) 'empty))
             (count 0)
             (current lst))
    (cond
      ((null? current) vect)
      (else (vector-set! vect count (car current))
            (loop vect (+ count 1) (cdr current))))))

(define (add-up-vectors vect1 vect2)
      (define new-vect (make-vector (vector-length vect1)))
      (define carry 0)
      (do ((pos (- (vector-length vect1) 1) (- pos 1))) ((< pos 0) new-vect)
        (let ((cnt (+ (vector-ref vect2 pos)
                      (vector-ref vect1 pos)
                      carry)))
          (vector-set! vect1 pos (if (>= cnt 10) (- cnt 10) cnt))
          (set! carry (if (>= cnt 10) 1 0)))))

(define (true-in-list? lst)
      (list? (member #t lst)))