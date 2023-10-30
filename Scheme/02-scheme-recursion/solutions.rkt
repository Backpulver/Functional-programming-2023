(define (sum-digits n)
  (define (itersum sum n)
    (if (< n 10)
        (+ sum (remainder n 10))
        (itersum (+ sum (remainder n 10)) (quotient n 10))))
  (itersum 0 n))

(define (count-divisors n)
  (define (iter count i n)
    (cond
      ((= 0 n) 0)
      ((> i n) count)
      ((= 0 (remainder n i)) (iter (+ count 1) (+ i 1) n))
      (else (iter count (+ i 1) n))))
  (iter 0 1 n))

(define (prime? n)
  (define (iter dividers i n)
    (cond
      ((> dividers 2) #f)
      ((or (= 0 n) (= 1 n)) #f)
      ((<= i n)
         (if (= 0 (remainder n i))
            (iter (+ dividers 1) (+ i 1) n)
            (iter dividers (+ i 1) n)))
      (else #t)))
  (iter 0 1 n))


        