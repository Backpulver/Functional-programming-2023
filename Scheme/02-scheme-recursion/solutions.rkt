(define (sum-digits n)
  (define (itersum sum n)
    (if (< n 10)
        (+ sum (remainder n 10))
        (itersum (+ sum (remainder n 10)) (quotient n 10))))
  (itersum 0 n))
      