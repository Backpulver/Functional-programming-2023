#lang racket

; 1
(define (len l)
  (define (helper sum l)
    (if (null? l)
        sum
        (helper (+ 1 sum) (cdr l))))
  (helper 0 l))

; 2
(define (any? p l)
  (if (null? l)
      #f
      (or (p (car l)) (any? p (cdr l)))))

(define (all? p l)
  (not (any? (lambda (x) (not (p x))) l)))

; 3
(define (member? x l)
  (cond
    ((null? l) #f)
    ((eqv? x (car l)) l)
    (else (member? x (cdr l)))))

; 4
(define (at n l)
  (define (helper pos n l)
    (cond
      ((null? l) #f)
      ((> pos n) #f)
      ((= pos n) (car l))
      (else (helper (+ 1 pos) n (cdr l)))))
  (helper 0 n l))

; 5
(define (my-map f l)
  (if (null? l)
      (list)
      (cons
        (f (car l))
        (my-map f (cdr l)))))

; 6
(define (filter p l)
  (cond
    [(null? l) (list)]
    [(p (car l)) (cons (car l) (filter p (cdr l)))]
    [else (filter p (cdr l))]))

; 7
(define (push x l)
  (if (null? l)
      (list x)
      (cons
        (car l)
        (push x (cdr l)))))

; 8
(define (reverse l)
  (if (null? l)
      (list)
      (append (reverse (cdr l)) (list (car l)))))

(define (reverse2 l)
  (foldl cons (list) l))