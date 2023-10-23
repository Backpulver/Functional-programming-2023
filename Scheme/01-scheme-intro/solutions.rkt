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
  (or (= -1 x)
      (= 1/3 x)))

(define (triangle? a b c)
  (and (< a (+ b c))
       (< b (+ a c))
       (< c (+ a b))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (fibonacci-tail n)
  (define (fib n a b)
    (if (= n 0)
        a
        (fib (- n 1) b (+ a b))))
  (fib n 0 1))