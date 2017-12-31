;;;
;; File:  chap1.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 02:47:12 2018
;;
#lang racket

;; 1.1.4 合成手続き

(define (square x) (* x x))

(module+ test
  (require rackunit)
  (check-eq? (square 2) 4)
  (check-eq? (square (+ 2 5)) 49)
  (check-eq? (square (square 3)) 81))

(define (sum-of-squres x y)
  (+ (square x) (square y)))

(module+ test
  (check-eq? (sum-of-squres 3 4) 25))

(define (f a)
  (sum-of-squres (+ a 1) (* a 2)))

(module+ test
  (check-eq? (f 5) 136))
