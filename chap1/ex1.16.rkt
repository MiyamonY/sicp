#lang racket

(define (fast-exp b n)
  (define (aux v b n)
    (cond ((= n 0) v)
          ((even? n) (aux v (* b b) (/ n 2)))
          (else (aux (* v b) b (- n 1)))))
  (aux 1 b n))

(module+ test
  (require rackunit)

  (check-eq? (fast-exp 2 0) 1)
  (check-eq? (fast-exp 2 3) 8)
  (check-eq? (fast-exp 2 10) 1024))
