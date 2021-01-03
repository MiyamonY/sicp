#lang racket

;; 行列計算になる
;; T = ((p + q, q), (q, p))
;; (a, b) = T (a, b)
;; T^2 = ((p+q, q), (q, p)) * ((p+q, q), (q, p))
;;     = ((p+q)^2 + q^2, 5)

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a b (+ (* p p) (* q q)) (+ (* q q) (* 2 p q)) (/ count 2)))
        (else
         (fib-iter (+ (* b q) (* a (+ p q)))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)))))

(module+ test
  (require rackunit)
  (check-eq? (fib 0) 0)
  (check-eq? (fib 1) 1)
  (check-eq? (fib 2) 1)
  (check-eq? (fib 3) 2)
  (check-eq? (fib 4) 3)
  (check-eq? (fib 10) 55))
