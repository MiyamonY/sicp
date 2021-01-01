#lang racket

(define (pascal n k)
  (cond ((<  n k) 0)
        ((= n 1) 1)
        ((or (= k 1) (= n k)) 1)
        (else
         (+ (pascal (- n 1) (- k 1))
            (pascal (- n 1) k)))))

(module+ test
  (require rackunit)

  (check-equal? (pascal 1 1) 1)
  (check-equal? (pascal 2 1) 1)
  (check-equal? (pascal 2 2) 1)
  (check-equal? (pascal 3 2) 2)

  (check-equal? (pascal 5 2) 4)
  (check-equal? (pascal 5 3) 6)
  (check-equal? (pascal 5 4) 4)
  (check-equal? (pascal 5 5) 1))
