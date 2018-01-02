;;;
;; File:  ex1.7.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 16:57:00 2018
;;
#lang racket

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough2? guess1 guess2)
  (< (abs (- guess1 guess2)) 1e-4))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt-iter2 guess1 guess2 x)
  (if (good-enough2? guess1 guess2)
      guess2
      (sqrt-iter2 guess2 (improve guess2 x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt2 x)
  (sqrt-iter2 0.0 1.0 x))

(module+ test
  (require rackunit)

  ;; 大きい場合: doubleの引き算で計算誤差（アンダーフロー）が発生する
  (check-true (< (abs (- (square 1e23) 1e46)) 1e-4))

  ;; 小さい場合: 既に計算した値との差が1e-4より小さいので、うまく計算でいない
  (check-= (sqrt 1e-6) 1e-3 1e-4)

  ;; 正常に計算できる
  (check-= (sqrt2 1e-6) 1e-3 1e-4))
