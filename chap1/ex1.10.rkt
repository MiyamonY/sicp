;;;
;; File:  ex1.10.rkt
;; Author: ymiyamoto
;;
;; Created on Wed Jan  3 03:43:06 2018
;;
#lang racket

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else
         (A (- x 1)
            (A x (- y 1))))))

(define (f n) (A 0 n))                  ; f(n) = 2 * n
(define (g n) (A 1 n))                  ; g(n) = 2 ** n
(define (h n) (A 2 n))                  ; h(n) = 2 ** (2 ** (2 ** ... ** 2))
(define (k n) (* 5 n n))                ; k(n) = 5 * n ** 2

(module+ test
  (require rackunit)

  (check-eq? (A 1 10) 1024)
  (check-eq? (A 2 4) 65536)
  (check-eq? (A 3 3) 65536)

  (check-eq? (f 5) 10)
  (check-eq? (f 10) 20)

  (check-eq? (g 5) 32)
  (check-eq? (g 10) 1024)

  (check-eq? (h 1) 2)
  (check-eq? (h 2) 4)
  (check-eq? (h 3) 16)                  ; 2 ** (2 ** 2)
  (check-eq? (h 4) 65536)               ; 2 ** ((2 ** 2) ** 2)
  )
