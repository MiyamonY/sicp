;;;
;; File:  ex1.11.rkt
;; Author: ymiyamoto
;;
;; Created on Thu Jan  4 09:43:30 2018
;;
#lang racket


(define (f n)
  (cond ((< n 3) n)
        (else
         (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(define (f-iter n)
  (define (aux a b c counter)
    (if (= counter n)
        a
        (aux (+ a (* 2 b) (* 3 c)) a b (+ counter 1))))
  (cond ((< n 3) n)
        (else
         (aux 2 1 0 2))))

(module+ test
  (require rackunit)
  (check-eq? (f 2) 2)
  (check-eq? (f 4) 11)

  (check-eq? (f-iter 2) 2)
  (check-eq? (f-iter 4) 11)

  (check-eq? (f 7) (f-iter 7)))
