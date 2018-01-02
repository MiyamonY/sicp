;;;
;; File:  ex1.9.rkt
;; Author: ymiyamoto
;;
;; Created on Wed Jan  3 03:37:49 2018
;;
#lang racket

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (+_rec a b)
  (if (= a 0)
      b
      (inc (+_rec (dec a) b))))

(module+ test
  (require rackunit)
  (check-eq? (+_rec 2 3) 5))

;; (+ 2 3)
;; -> (inc (+ 1 3))
;; -> (inc (inc (+ 0 3)))
;; -> (inc (inc 3))
;; -> (inc 4)
;; -> 5

(define (+_iter a b)
  (if (= a 0)
      b
      (+_iter (dec a) (inc b))))

(module+ test
  (check-eq? (+_iter 2 3) 5))

;; (+ 2 3)
;; -> (+ 1 4)
;; -> (+ 0 5)
;; -> 5
