#lang racket

(define (f-rec n)
  (cond ((<= n 3) n)
        (else
         (+ (f-rec (- n 1) ) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3)))))))

(define (f-iter n)
  (define (aux a b c m)
    (if (= n m)
        a
        (aux (+ a (* 2 b) (* 3 c)) a b (+ 1 m))))

  (if (<= n 3)
      n
      (aux 3 2 1 3)))

(module+ test
  (require rackunit)
  (check-eq? (f-rec 1) 1)
  (check-eq? (f-rec 2) 2)
  (check-eq? (f-rec 3) 3)
  (check-eq? (f-rec 4) 10)
  (check-eq? (f-rec 5) 22)
  (check-eq? (f-rec 6) 51)
  (check-eq? (f-rec 7) 125)

  (check-eq? (f-iter 2) 2)
  (check-eq? (f-iter 3) 3)
  (check-eq? (f-iter 6) 51)
  (check-eq? (f-iter 7) 125))
