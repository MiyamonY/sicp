#lang racket

(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (prod a b)
  (cond ((= b 0) 0)
        ((even? b) (prod (double a) (halve b))) ; a * b = 2a * b/2
        (else
         (+ a (prod a (- b 1))))))      ; a * b = a + a*(b-1)

(module+ test
  (require rackunit)

  (check-eq? (prod 3 2) 6)
  (check-eq? (prod 4 5) 20))
