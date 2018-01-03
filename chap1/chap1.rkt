;;;
;; File:  chap1.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Jan  1 02:47:12 2018
;;
#lang racket

;; 1.1.4 合成手続き

(define (square x) (* x x))

(module+ test
  (require rackunit)
  (check-eq? (square 2) 4)
  (check-eq? (square (+ 2 5)) 49)
  (check-eq? (square (square 3)) 81))

(define (sum-of-squres x y)
  (+ (square x) (square y)))

(module+ test
  (check-eq? (sum-of-squres 3 4) 25))

(define (f a)
  (sum-of-squres (+ a 1) (* a 2)))

(module+ test
  (check-eq? (f 5) 136))

;; 1.1.6 条件式と述語

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(module+ test
  (check-eq? (abs 3) 3)
  (check-eq? (abs 0) 0)
  (check-eq? (abs -5) 5))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else  x)))

(module+ test
  (check-eq? (abs2 2) 2)
  (check-eq? (abs2 -3) 3))

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

(module+ test
  (check-eq? (abs3 1) 1)
  (check-eq? (abs3 -6) 6))

(define (>= x y)
  (not (< x y)))

(module+ test
  (check-true (>= 3 3))
  (check-true (>= 5 2))
  (check-false (>= 2 4)))

;; 1.1.7 例.Newton法による平方根
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(module+ test
  (check-= (sqrt 9) 3.0 0.0001)
  (check-= (sqrt (+ 100 37)) 11.704699 0.001))

;; 1.1.8 ブラックボッス抽象としての手続き

(define (sqrt2 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 1e-4))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(module+ test
  (check-= (sqrt2 2) 1.414121356 1e-4))

;; 1.2 手続きとその生成するプロセス
;; 1.2.1 線形再帰と反復
(define (factorial n)
  (if (= n 1 )
      1
      (* n (factorial (- n 1)))))

(module+ test
  (check-eq? (factorial 4) 24))

(define (ifactorial n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* product counter) (+ counter 1))))
  (fact-iter 1 1))

(module+ test
  (check-eq? (ifactorial 4) 24))

;; 1.2.2 木構造再帰
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(module+ test
  (check-eq? (fib 3) (fib2 3))
  (check-eq? (fib 7) (fib2 7)))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(module+ test
  (check-eq? (count-change 0) 1)
  (check-eq? (count-change 3) 1)
  (check-eq? (count-change 15) 6))
