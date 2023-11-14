(define (++ x)
  (+ 1 x))

(define (dividable? n i)
  (= 0 (remainder n i)))

(define (prime? n)
  (define (helper divisor)
    (cond
      ((< n 2) #f)
      ((= divisor n) #t)
      ((dividable? n divisor) #f)
      (else (helper (++ divisor)))))
  (helper 2))

(define (product-of-prime-divisors n)
  (define (helper prod i)
    (cond
      ((= i n) prod)
      ((and (dividable? n i) (prime? i)) (helper (* prod i) (+ 1 i)))
      (else (helper prod (+ 1 i)))))
  (helper 1 2))

(define (trim n)
  (/ n (product-of-prime-divisors n)))

(define (unitary-divisor? n k)
  (define (helper divisor q k)
    (cond
      ((> divisor (min q k)) #t)
      ((and (prime? divisor) (= 0 (remainder q divisor)) (= 0 (remainder k divisor))) #f)
      (else (helper (+ 1 divisor) q k))))
  (helper 1 (/ n k) k))


(define (commonUnitary n1 n2)
  (define (helper divisor count)
    (cond
      ((> divisor (min n1 n2)) count)
      ((and (unitary-divisor? n1 divisor) (unitary-divisor? n2 divisor)) (helper (+ 1 divisor) (+ 1 count)))
      (else (+ 1 divisor) count)))
  (helper 1 0))