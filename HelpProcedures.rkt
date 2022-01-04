;; Executes a function on each element of the given data-structure
(define (map-object function lst)
  (map function lst))

(define (for-each-object function lst)
  (for-each function lst))

(define (remove-from-list element lst)
  (let ((search-list (reverse lst)))
    (let search-and-remove ((current (car search-list))
                            (remaining (cdr search-list))
                            (result '()))
      (cond
        ((and (null? remaining) (eq? current element)) result)
        ((null? remaining) (cons current result))
        ((eq? current element) (append result remaining))
        (else (search-and-remove (car remaining) (cdr remaining) (cons current result)))))))