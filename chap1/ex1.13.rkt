#lang racket

(define (pow e n)
  (if (= n 0)
      1
      (* e (pow e (- n 1)))))

(define (phi n)
  (pow (/ (+ 1 (sqrt 5)) 2) n))

(define (psi n)
  (pow (/ (- 1 (sqrt 5)) 2) n))

(define (fib n)
  (round (/ (phi n) (sqrt 5))))

(module+ test
  (require rackunit)

  ;; phi^2 = (3+sqrt 5)/ 2 より Fib(n) = phi^n - psi^n / sqrt 5が言える．
  ;; abs (psi^n) < 1より，題意が示せた
  (check-true (< (abs (psi 1)) 1.0)))
