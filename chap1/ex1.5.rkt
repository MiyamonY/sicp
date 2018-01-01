;;;
;; File:  ex1.5.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 16:18:30 2018
;;
#lang racket

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;; 作用的順序
;; (test 0 (p))
;; -> (test 0 (p))
;; -> (test 0 (p))
;; -> ...

;; 正規順序
;; (test 0 (p))
;; -> (if (= 0 0) 0 (p))
;; -> (if #t 0 (p))
;; -> 0
