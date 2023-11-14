(define (id x) x)

(define (1+ x) (+ x 1))

(define (compose f g) (lambda (x) (f (g x))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

; 1 a)
(define (sum-digits n)
  (define (helper sum n)
    (if (= 0 n)
        sum
        (helper (+ sum (remainder n 10)) (quotient n 10))))
  (helper 0 n))

(define (endurance n)
  (define (iter sum n)
    (if (< n 10)
        sum
        (iter (+ 1 sum) (sum-digits n))))
  (iter 0 (abs n)))

; 1 b)
(define (min-endurance-of-range a b)
  (define (helper_ min i)
    (cond
      ((> i b) min)
      ((<= (endurance i) min) (helper_ (endurance i) (+ 1 i)))
      (else (helper_ min (+ 1 i)))))
  (helper_ (endurance a) a))

(define (max-number-in-list l)
  (define (helper max l)
    (cond
      ((null? l) max)
      ((> (car l) max) (helper (car l) (cdr l)))
      (else (helper max (cdr l)))))
  (helper (car l) (cdr l)))

(define (min-endurance-max-length a b)
  (define (iter min-endurance l-nums a b)
    (cond
      ((> a b) l-nums)
      ((= (endurance a) min-endurance) (iter min-endurance (append l-nums (list a)) (+ 1 a) b))
      (else (iter min-endurance l-nums (+ 1 a) b))))
  (max-number-in-list (iter (min-endurance-of-range a b) '() a b) ))

; 3
(define (avg-of-list l)
  (define (helper sum elems l)
    (if (null? l)
        (list (/ sum elems))
        (helper (+ sum (car l)) (+ 1 elems) (cdr l))))
  (helper 0 0 l))
  
(define (averages l)
  (define (iter newl l)
    (if (null? l)
        newl
        (iter (append newl (avg-of-list l)) (cdr l))))
  (iter '() l))
