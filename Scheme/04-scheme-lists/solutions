(define (len l)
  (define (helper sum l)
    (if (null? l)
        sum
        (helper (+ 1 sum) (cdr l))))
  (helper 0 l))

(define (any? p l)
  (if (null? l)
      #f
      (or (p (car l)) (any? p (cdr l)))))

(define (all? p l)
  (define (helper status p l)
    (cond
      ((null? l) #f)
      ((not status) #f)
      ((not (p l)) #f)
      (else helper #t p (cdr l))))
  (helper (p (car l)) (cdr l)))