(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))


(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate-i
       op
       (op nv (term a))
       (next a)
       b
       term
       next)))

(define ++
  (lambda (a) (+ a 1)))

(define (square x)
  (* x x))

; 1
(define (o f g)
  (lambda (x) (f (g x))))

; 2
(define (repeated n f x)
  (if (< n 1)
      x
      (repeated (- n 1) f (f x))))

; 3
(define (repeat n f)
  (lambda (x)
    (repeated n f x)))

(define (dividable? n x)
  (= 0 (remainder n x)))

(define (prime? n)
  (define (helper divisor)
    (cond
      ((< n 2) #f)
      ((= divisor n) #t)
      ((dividable? n divisor) #f)
      (else (helper (+ divisor 1)))))
  (helper 2))

; 4
(define (count p a b)
  (accumulate + 0 a b (lambda (x) (if (p x) 1 0)) (lambda (x) (+ x 1))))

; 5
(define (any? p a b)
  (accumulate (lambda (term i) (or term i)) #f a b p ++))

; 6
(define (all? p a b)
  (accumulate (lambda (term i) (and term i)) #t a b p ++))


(define (id x) x)

; 7 
(define (repeated-accum n f x)
  (accumulate (lambda (term i) (f i)) x 1 n id ++))

; 8 i have no idea what this does, test ((repeat-accum 4 ++) 4)
(define (repeat-accum n f)
  (accumulate o id 1 n (lambda (_) f) ++))