;;;
;; File:  ex1.3.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 16:09:09 2018
;;
#lang racket

(define (square x) (* x x))

(define (sum-of-bigger2 a b c)
  (square (- (+ a b c) (min a b c))))

(module+ test
  (require rackunit)
  (check-eq? (sum-of-bigger2 1 2 3) 25)
  (check-eq? (sum-of-bigger2 5 4 3) 81))
