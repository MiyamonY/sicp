;;;
;; File:  ex1.2.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 03:31:42 2018
;;
#lang racket

(module+ test
  (require rackunit)

  (check-eqv? (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
                 (* 3 (- 6 2) (- 2 7)))
             (- (/ 37 150))))
