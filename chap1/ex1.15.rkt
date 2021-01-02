#lang racket

(define (cube n ) (* n n n))
(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (count-less-than-0.1 a)
  (define (aux a count)
    (if (< a 0.1) count
        (aux (/ a 3) (+ count 1))))
  (aux a 0))

(module+ test
  (require rackunit)
  (check-within (sine 12.5) -0.0608 0.001)

  ;; a.手続きpを1回適用する度に1/3となるので，5回
  (check-equal? (count-less-than-0.1 12.5) 5)
  ;; b.a/(3^n) < 0.1となる，計算量・スペース共にn = log_3(10a) に比例する
  )
