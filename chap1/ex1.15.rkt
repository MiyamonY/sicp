;;;
;; File:  ex1.15.rkt
;; Author: ymiyamoto
;;
;; Created on Thu Jan  4 14:57:31 2018
;;
#lang racket

(define n 0)

(define (cube x) (* x x x))
(define (p x)
  (set! n (add1 n))
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))


(module+ test
  (require rackunit)
  (sine 12.5)
  (check-eq? n 5)

  ;; O(log3(a))
  )
