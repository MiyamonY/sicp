;;;
;; File:  ex1.16.rkt
;; Author: ymiyamoto
;;
;; Created on Fri Jan  5 06:46:02 2018
;;
#lang racket

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (expt-iter (* b b) (/ counter 2) product))
        (else
         (expt-iter b (- counter 1) (* b product)))))

(module+ test
  (require rackunit)
  (check-eq? (expt 2 3) 8)
  (check-eq? (expt 3 3) 27))
