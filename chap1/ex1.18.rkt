#lang racket

(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (prod a b)
  (define (aux v a b)
    (cond ((= b 0) v)
          ((even? b) (aux v (double a) (halve b)))
          (else
           (aux (+ a v) a (- b 1)))))   ; a * b = a + a * (b-1)
  (aux 0 a b))

(module+ test
  (require rackunit)

  (check-eq? (prod 3 8) 24)
  (check-eq? (prod 2 3) 6))
