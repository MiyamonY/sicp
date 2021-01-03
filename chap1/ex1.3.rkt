;;;
;; File:  ex1.3.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 16:09:09 2018
;;
#lang racket

(define (square x) (* x x))

(define (sum-of-square-bigger2 a b c)
  (cond ((and (>= a c) (>= b c)) (+ (square a) (square b)))
        ((and (>= b a) (>= c a)) (+ (square b) (square c)))
        ((and (>= c b) (>= a b)) (+ (square c) (square a)))))

(module+ test
  (require rackunit)
  (check-eq? (sum-of-square-bigger2 1 2 3) 13)
  (check-eq? (sum-of-square-bigger2 2 3 1) 13)
  (check-eq? (sum-of-square-bigger2 3 2 1) 13)
  (check-eq? (sum-of-square-bigger2 3 3 2) 18)
  (check-eq? (sum-of-square-bigger2 5 4 3) 41))
