#lang racket

((collect-pre-order (t-left t)) the-empty-tree empty) ; moje vmesto empty -> (list)
(define e the-empty-tree)

(define (make-tree root left right)
  (list root left right))
(define mk make-tree)

(define (make-leaf-tree root)
  (mk root e e))

(define mkl make-leaf-tree)

(define t-root car)
(define t-left cadr)
(define t-right caddr)
(define (t-empty? t)
  (equal? t e))

(define (t-leaf?)
  (and (not (t-empty? t))
       (t-empty? (t-left t))
       (t-empty? (t-right))))

(define (tree? t)
  (or (t-empty? t)
      (and
       (= 3 (length t))
       (tree? (card t))
       (tree? (caddr t)))))

(define example-tree
  (mk 1
      (mkl 2)
      (mk 3
          (mkl 4)
          (mk 5
              e
              (mkl 6)))))

;; 2
(define (collect-pre-order t)
  (if (t-empty? t)
      (list)
      (if (t-leaf? t)
          (list (t-root t))
          (append
            (list (t-root t))
            (collect-pre-order (t-left t))
            (collect-pre-order (t-left t))))))

(define (collect-in-order t)
  (if (t-empty? t)
      (list)
      (append
        (collect-in-order (t-left t))
        (list (t-root? t))
        (collect-in-order (t-right t)))))

(define (collect-post-order t)
  (if (t-empty? t)
      (list)
      (append
        (collect-in-order (t-left t))
        (collect-in-order (t-right t))
        (list (t-root? t)))))

;; assoc
(define example-al
  (list
    (cons 1 2)
    (cons 1 "a")
    (cons 2 5)
    (cons -3 "aloda")))

;; assoc, assv, assq
(define (zipWith f l1 l2)
  (map f l1 l2))

(define (index l)
  (zipWith
     cons
     (range (length l))
     l))

(define (put k v al)
  (if (empty? al)
      (list (cons k v))
      (let* ([k1 (caar al)])
        (if (equal? k k1)
            (cons
              (cons k v)
              tail)
            (cons
              head
              (put k v tail))))))

(define any? ormap)

(define (put2 kv al)
  (let ([keys (mp car al)])
     (if (any? (lambda (key) (equal? k key)) keys)
         (map
           (lambda (kvp)
              (if (equal? (car kvp))
                  (cons k v)
                  kvp))
           al)
         (cons (cons k v) al))))