;;;
;; File:  ex1.12.rkt
;; Author: ymiyamoto
;;
;; Created on Thu Jan  4 09:55:10 2018
;;
#lang racket

(define (pascal x y)
  (cond ((= x 1) 1)
        ((= y 1) 1)
        ((= y x) 1)
        (else
         (+ (pascal (- x 1) (- y 1))
            (pascal (- x 1) y)))))

(module+ test
  (require rackunit)
  (check-eq? (pascal 1 1) 1)
  (check-eq? (pascal 5 1) 1)
  (check-eq? (pascal 5 3) 6)
  (check-eq? (pascal 5 4) 4))
