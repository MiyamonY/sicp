;;;
;; File:  ex1.4.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 16:12:30 2018
;;
#lang racket

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(module+ test
  (require rackunit)
  (check-eqv? (a-plus-abs-b 3 4) 7)
  (check-eqv? (a-plus-abs-b 3 -4) 7))

;; (a-plus-abs-b 3 4)
;; -> ((if (> 4 0) + - ) 3 4)
;; -> ((if #t + -) 3 4)
;; -> (+ 3 4)
;; -> 7
