(define (zad0.1)
  (* (+ 10 5.16 19 9.712361)
     (- 20
        (- 16 4))))

(define (zad0.2)
  (+ 1/4
     2/5
     3/8
     (* 6
        (- 5.1 1.6)
        (- 9/3 7/4))))

(define (zad0.3)
  (+ (expt 3
           (/ 60 7))
     (/ (expt 2 10)
        179)))

(define (zad0.4)
  (expt 1-i 21))

(define (add a b)
  (+ a b))

(define (is-even? number)
  (= (remainder number 2) 0))

(define (signum number)
  (cond
    ((<= number -1) -1)
    ((= number 0) 0)
    ((>= number 1) 1)))

(define (root? x)
  