;;;
;; File:  ex1.6.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 16:49:44 2018
;;
#lang racket

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(module+ test
  (require rackunit)
  (check-eq? (new-if (= 2 3) 0 5) 5)
  (check-eq? (new-if (= 1 1) 0 5) 0))

;; new-ifはspecial formではないため、new-ifの評価の前にelse-clauseを評価する。
;; そのため、predicateが#trueとなる条件でもsqr-iterが毎回評価されるので、
;; 無限ループに陥いる。
