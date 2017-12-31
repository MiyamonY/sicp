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

;; 1.1.6 条件式と述語

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(module+ test
  (check-eq? (abs 3) 3)
  (check-eq? (abs 0) 0)
  (check-eq? (abs -5) 5))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else  x)))

(module+ test
  (check-eq? (abs2 2) 2)
  (check-eq? (abs2 -3) 3))

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

(module+ test
  (check-eq? (abs3 1) 1)
  (check-eq? (abs3 -6) 6))

(define (>= x y)
  (not (< x y)))

(module+ test
  (check-true (>= 3 3))
  (check-true (>= 5 2))
  (check-false (>= 2 4)))
