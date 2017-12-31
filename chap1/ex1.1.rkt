;;;
;; File:  ex1.1.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 03:26:35 2018
;;
#lang racket

(module+ test
  (require rackunit)

  (check-eq? (+ 5 3 4) 12)
  (check-eq? (- 9 1) 8)
  (check-eq? (/ 6 2) 3)
  (check-eq? (+ (* 2 4) (- 4 6)) 6)
  (define a 3)
  (define b (+ a 1))
  (check-eq? (+ a b (* a b)) 19)
  (check-false (= a b))

  (check-eq? (if (and (> b a) (< b (* a b)))
                 b
                 a) 4)
  (check-eq? (cond ((= a 4) 6)
                   ((= b 4) (+ 6 7 a))
                   (else 25))
             16)
  (check-eq? (+ 2 (if (> b a) b a)) 6)
  (check-eq? (* (cond ((> a b) a)
                      ((< a b) b)
                      (else -1))
                (+ a 1))
             16))
