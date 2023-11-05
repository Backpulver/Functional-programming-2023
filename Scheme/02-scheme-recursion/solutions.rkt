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

(define (increasing-digits? n)
  (cond
    ((< n 10) #t))
  (define (iter prev_digit n)
    (cond
      ((< n 10)
       (if (< prev_digit n)
           #f
           #t))
      ((< prev_digit (remainder n 10)) #f)
      (else (iter (remainder n 10) (quotient n 10)))))
  (iter (remainder n 10) (quotient n 10)))

(define (ends-with? n k)
  (cond
    ((< n k) #f)
    ((= n k) #t))
  (define (iter last_digit_n last_digit_k n k)
    (cond
      ((and (= 0 k) (= last_digit_n last_digit_k)) #t) 
      ((not (= last_digit_n last_digit_k)) #f)
      (else (iter (remainder n 10) (remainder k 10) (quotient n 10) (quotient k 10)))))
  (iter (remainder n 10) (remainder k 10) (quotient n 10) (quotient k 10)))


(define (automorphic? n)
  (ends-with? (expt n 2) n))

(define (perfect? n)
  (define (iter sum divider n)
    (cond
      ((> divider (+ n 1)) #f)
      ((= sum (+ n 1) divider) #t)
      ((= 0 (remainder (+ n 1) divider)) (iter (+ sum divider) (+ divider 1) n))
      (else (iter sum (+ divider 1) n))))
  (iter 0 1 (- n 1)))

(define (binary-to-decimal n)
  (define (iter sum power n)
    (cond
      ((not (or (= 0 (remainder n 10)) (= 1 (remainder n 10)))) "Not a binary number!")
      ((= n 0) sum)
      (else (iter (+ sum (* (expt 2 power) (remainder (remainder n 10) 2))) (+ power 1) (quotient n 10)))))
  (iter 0 0 n))

(define (decimal-to-binary n)
  (define (iter binary_number times n)
    (if (= n 0)
        binary_number
        (iter (+ binary_number (* (expt 10 times) (remainder n 2))) (+ times 1) (quotient n 2))))
  (iter 0 0 n))
                 