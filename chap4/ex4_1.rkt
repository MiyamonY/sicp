;;;
;; File:  ex4_1.rkt
;; Author: ymiyamoto
;;
;; Created on Wed Oct 25 21:56:08 2017
;;
#lang racket

(define (list-of-value exps env)
  (define (no-operands? ops) (null? ops))
  (if (no-operands? exps)
      (list)
      (cons (eval (car exps) env) (list-of-value (cdr exps env)))))

(define (list-of-value-left exps env)
  (define (no-operands? ops) (null? ops))
  (if (no-operands? exps)
      (list)
      (let ((first (eval (car exps) env)))
        (cons first (list-of-value (cdr exps env))))))

(define (list-of-value-right exps env)
  (define (no-operands? ops) (null? ops))
  (if (no-operands? exps)
      (list)
      (let ((rest (list-of-value (cdr exps env))))
        (cons (eval (car exps) env) rest))))
