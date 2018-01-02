;;;
;; File:  ex1.8.rkt
;; Author: ymiyamoto
;;
;; Created on Tue Jan  2 18:07:21 2018
;;
#lang racket

(define (triple x) (* x x x))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (triple guess) x)) 1e-4))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x) x)))

(define (cube x)
  (cube-iter 1.0 x))

(module+ test
  (require rackunit)
  (check-= (cube 1) 1 1e-4)
  (check-= (cube 64) 4.0 1e-4)
  (check-= (cube 2) 1.25999210 1e-4))
