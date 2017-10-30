;;;
;; File:  text.rkt
;; Author: ymiyamoto
;;
;; Created on Wed Oct 25 20:35:38 2017
;;
#lang racket

(module+ test
  (require rackunit)
  (require rackunit/text-ui))

;; 4.1
(module util racket
  (provide tagged-list? underlying-apply)

  (define underlying-apply apply)

  (define (tagged-list? exp tag)
    (cond ((pair? exp) (eq? (car exp) tag))
          ((mpair? exp) (eq? (mcar exp) tag)))))

(module+ test
  (require (submod ".." util))

  (run-tests
   (test-suite "tagged-list?"
               (test-true "tagged"
                          (tagged-list? '(aaa 1 2 3) 'aaa))
               (test-false "not tagged"
                           (tagged-list? '(bbb 1 2 3) 'aaa)))))

(module env racket
  (provide set-variable-value!
           define-variable!
           extend-environment
           lookup-variable-value
           setup-environment)

  (require (submod ".." util))
  (require compatibility/mlist)

  (define primitive-procedures
    (mlist (mlist 'car car)
           (mlist 'cdr cdr)
           (mlist 'cons cons)
           (mlist 'null? null?)
           (mlist 'list list)
           (mlist '= =)
           (mlist '+ +)
           (mlist '- -)
           (mlist '* *)))

  (define primitive-procedure-names (mmap mcar primitive-procedures))
  (define primitive-procedure-objects
    (mmap (lambda (proc) (mlist 'primitive (mcar (mcdr proc))))
          primitive-procedures))

  (define the-empty-environment '())

  (define (enclosing-environment env) (mcdr env))
  (define (first-frame env) (mcar env))

  (define (make-frame variables values)
    (mcons variables (mlist values)))

  (define (frame-variables frame) (mcar frame))
  (define (frame-values frame) (mcar (mcdr frame)))

  (define (add-binding-to-frame! var val frame)
    (set-mcar! frame (mcons var (mcar frame)))
    (set-mcdr! frame (mcons (mcons val (mcar (mcdr frame))) '())))

  (define (extend-environment vars vals base-env)
    (if (= (mlength vars) (mlength vals))
        (mcons (make-frame vars vals) base-env)
        (if (< (mlength vars) (mlength vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals))))

  (define (lookup-variable-value var env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((not (mpair? vars))
               (env-loop (enclosing-environment env)))
              ((eq? var (mcar vars)) (mcar vals))
              (else (scan (mcdr vars) (mcdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))

  (define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((not (mpair? vars))
               (env-loop (enclosing-environment env)))
              ((eq? var (mcar vars))
               (set-mcar! vals val))
              (else (scan (mcdr vars) (mcdr vals)))))
      (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
    (env-loop env))

  (define (define-variable! var val env)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((not (mpair? vars))
               (add-binding-to-frame! var val frame))
              ((eq? var (mcar vars))
               (set-mcar! vals val))
              (else
               (scan (mcdr vars) (mcdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame))))

  (define (setup-environment)
    (let ((initial-env (extend-environment
                        primitive-procedure-names
                        primitive-procedure-objects
                        the-empty-environment)))
      (define-variable! 'true true initial-env)
      (define-variable! 'false false initial-env)
      initial-env)))

(module+ test
  (require (submod ".." env))
  (require compatibility/mlist)

  (define env (setup-environment))
  (define (reset-env) (set! env (setup-environment)))

  (run-tests
   (test-suite
    "test env"
    (test-suite
     "lookup-variable-value"
     #:before (lambda () (set! env
                               (extend-environment
                                (mlist 'b 'c) (mlist 4 5)
                                (setup-environment))))
     #:after reset-env
     (test-eqv? "variable found"
                (lookup-variable-value 'c env) 5)
     (test-equal? "varialbe found in deep frame"
                  (lookup-variable-value '+ env) (mlist 'primitive +))
     (test-exn "variable not found"
               exn:fail?
               (lambda () (lookup-variable-value 'w env))))
    (test-suite
     "set-variable-value!"
     #:before (lambda ()
                (set! env (extend-environment (mlist 'x) (mlist '10) env))
                (set-variable-value! 'x 5 env))
     #:after reset-env
     (test-eqv? "variable is set"
                (lookup-variable-value 'x env) 5)
     (test-exn "not defined variable raise error" exn:fail?
               (lambda () (set-variable-value! 'w 3 env))))
    (test-suite
     "define-variable!"
     #:before (lambda ()
                (define-variable! 'b 10 env))
     #:after reset-env
      (test-eqv? "defined variable found"
                 (lookup-variable-value 'b env) 10)))))

(module syntax racket
  (provide self-evaluating?
           variable?
           assignment?
           assignment-variable
           assignment-value
           definition?
           definition-variable
           quated?
           begin?
           begin-actions
           lambda?
           lambda-parameters
           lambda-body
           primitive-procedure?
           primitive-implementation
           procedure-parameters
           procedure-body
           procedure-environment
           if?
           if-predicate
           if-consequent
           if-altenative
           application?
           compound-procedure?
           cond?
           cond-else-clause?
           cond-clauses
           cond-predicate
           cond-actions
           operator
           operands
           last-exp?
           first-exp
           rest-exps)
  (require (submod ".." util))

  (define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))

  (define (variable? exp) (symbol? exp))

  (define (assignment? exp)
    (tagged-list? exp 'set!))
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))

  (define (definition? exp)
    (tagged-list? exp 'define))
  (define (definition-variable exp)
    (cadr exp))

  (define (quated? exp)
    (tagged-list? exp 'quate))

  (define (begin? exp) (tagged-list? exp 'begin))
  (define (begin-actions exp) (cdr exp))

  (define (if? exp) (tagged-list? exp 'if))
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-altenative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

  (define (lambda? exp) (tagged-list? exp 'lambda))
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))

  (define (application? exp) (pair? exp))

  (define (primitive-procedure? proc) (tagged-list? proc 'primitive))
  (define (primitive-implementation proc) (mcar (mcdr proc)))

  (define (procedure-parameters p) (cadr p))
  (define (procedure-body p) (caddr p))
  (define (procedure-environment p) (cadddr p))

  (define (compound-procedure? p) (tagged-list? p 'procedure))

  (define (cond? exp) (tagged-list? exp 'cond))
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))

  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))

  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp exps) (car exps))
  (define (rest-exps exps) (cdr exps)))

(module+ test
  (require (submod ".." syntax))
  (run-tests
   (test-suite
    "test syntax"
    (test-suite
     "self-evaluating?"
     (test-true "number is self-evaluating" (self-evaluating? '1))
     (test-true "string is self-evaluating" (self-evaluating? '"aaa"))
     (test-false "other is not self-evaluating" (self-evaluating? (cons 1 "aaa"))))
    (test-suite
     "variable"
     (test-true "variable" (variable? 'zz))
     (test-false "number is not varaible" (variable? 1)))
    (test-suite
     "assingment"
     (let ((assignment '(set! x 3)))
       (test-true "assignment format" (assignment? assignment))
       (test-eq? "assignment varaible" (assignment-variable assignment) 'x)
       (test-eq? "assignment value" (assignment-value assignment) 3)))
    )))

(module eval racket
  (provide eval)

  (require compatibility/mlist)
  (require (submod ".." syntax))
  (require (submod ".." env))
  (require (submod ".." util))

  (define (false? x) (eq? x false))
  (define (true? x) (not (false? x)))

  (define (text-of-quotation exp) (cadr exp))

  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)

  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval (assignment-value exp) env)
      env)
    'ok)

  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-altenative exp) env)))

  (define (make-procedure parameters body env)
    (list 'procedure parameters body env))

  (define (make-begin exps)
    (list 'begin exps))

  (define (make-if pred exp1 exp2) (list 'if pred exp1 exp2))

  (define (eval-sequence exps env)
    (cond ((last-exp? exps)
           (eval (first-exp exps) env))
          (else
           (eval (first-exp exps) env)
           (eval-sequence (rest-exps exps) env))))

  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else
           (make-begin seq))))

  (define (cond->if exp)
    (define (expand-clauses clauses)
      (if (null? clauses)
          'false
          (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF" clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))
    (expand-clauses (cond-clauses exp)))


  (define (apply-in-underlying-scheme proc args)
    (underlying-apply proc args))

  (define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme
     (primitive-implementation proc) args))

  (define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (eval-sequence (procedure-body procedure)
                          (extend-environment
                           (list->mlist (procedure-parameters procedure))
                           (list->mlist arguments)
                           (procedure-environment procedure))))
          (else
           (error "Unknown procedure type -- APPLY" procedure))))

  (define (eval exp env)
    (define (list-of-values exps env)
      (define (no-operands? ops) (null? ops))
      (define (first-operand ops) (car ops))
      (define (rest-operands ops) (cdr exps))
      (if (no-operands? exps)
          (list)
          (cons (eval (first-operand exps) env)
                (list-of-values (rest-operands exps) env))))
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quated? exp) (text-of-quotation exp))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((lambda? exp) (make-procedure (lambda-parameters exp)
                                         (lambda-body exp)
                                         env))
          ((begin? exp)
           (eval-sequence (begin-actions exp) env))
          ((cond? exp)
           (eval (cond->if exp) env))
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else
           (error "Unknown expression type -- EVAL" exp)))))

(module+ test
  (require (submod ".." eval))
  (require (submod ".." env))

  (define test-env (setup-environment))
  (define-variable! 'x 10 test-env)

  (run-tests
   (test-suite
    "test env"
    (test-suite
     "env"
     (test-eqv? "eval number" (eval '3 test-env) 3)
     (test-eqv? "eval string" (eval "aaa" test-env) "aaa")
     (test-suite
      "eval variable"
      (test-eqv? "variable found" (eval 'x test-env) 10)
      (test-exn "eval not defined variable" exn:fail?
                (lambda () (eval 'w test-env))))
     (test-eq? "eval quated" (eval '(quate a) test-env) 'a)
     (test-eqv? "eval assignment" (eval '(begin (set! x (+ 3 5)) x) test-env) 8)
     (test-eqv? "eval definition" (eval '(begin (define w "aaa") w) test-env) "aaa")
     (test-suite
      "eval if"
      (test-eqv? "predicate true" (eval '(if true 1) test-env) 1)
      (test-eqv? "predicate false" (eval '(if false 1 2) test-env) 2)
      (test-eq? "else clause not exist" (eval '(if false 1) test-env) false))
     (test-equal? "eval lambda" (eval '(lambda (x y) (+ x y)) test-env)
                  (list 'procedure '(x y) '((+ x y)) test-env))
     (test-equal? "eval begin" (eval '(begin
                                        (define z 100)
                                        (set! z 30)
                                        z) test-env) 30)
     (test-suite
      "eval cond"
      (test-eqv? "first clause" (eval '(cond (true 1) (else 3)) test-env) 1)
      (test-eqv? "not first clause" (eval '(cond (false 2)
                                            (false "aaa")
                                            ((= 1 1) (+ 1 2))
                                            (else (* 1 2))) test-env) 3)
      (test-eqv? "eval else clause" (eval '(cond (false 2)
                                                 (false "aaa")
                                                 ((= 1 3) (+ 1 2))
                                                 (else (+ 10 2))) test-env) 12))
     (test-suite
      "eval pplication"
      (test-eqv? "primitive function" (eval '(= 1 1) test-env) true)
      (test-exn "undefined function" exn:fail?
                (lambda () (eval '(aaa 1 2) test-env)))
      (test-eqv? "defined funciton"
                 (eval '(begin
                          (define sum (lambda (x y) (+ x y)))
                          (sum (+ 1 0) (* 1 2)))
                       test-env)
                 3))
     (test-suite
      "test complex function"
      (test-eqv? "fact" (eval '(begin
                                 (define fact
                                   (lambda (n)
                                     (if (= n 1 )
                                         1
                                         (* n (fact (- n 1))))))
                                 (fact (* 2 2)))
                              test-env) 24)
      (test-eqv? "fib" (eval '(begin
                                (define fib
                                  (lambda (n)
                                    (cond
                                      ((= n 0) 0)
                                      ((= n 1) 1)
                                      (else (+ (fib (- n 1))
                                               (fib (- n 2)))))))
                                (fib (* 5 2)))
                             test-env) 55)
      (test-equal? "list lengths by map"
                 (eval '(begin
                          (define length
                            (lambda (l)
                              (if (null? l)
                                  0
                                  (+ 1 (length (cdr l))))))
                          (define map
                            (lambda (proc l)
                              (if (null? l)
                                  (list)
                                  (cons (proc (car l)) (map proc (cdr l))))))
                          (map length (quate ((a) (a b) (a b c) (a b c d)))))
                       test-env) (list 1 2 3 4)))))))

(module repl racket
  (require (submod ".." env))
  (require (submod ".." syntax))
  (require (submod ".." eval))
  (provide driver-loop)

  (define input-prompt ";;; M-Eval input:")
  (define output-prompt ";;; M-Eval value:")

  (define (prompt-for-input string)
    (newline) (newline) (display string) (newline))

  (define (announce-output string)
    (newline) (display string) (newline))

  (define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                       (procedure-parameters object)
                       (procedure-body object)))
        (display object)))

  (define the-global-environment (setup-environment))

  (define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input  (read)))
      (let ((output (eval input the-global-environment)))
        (announce-output output-prompt)
        (user-print output)))
    (driver-loop)))

(module* main #f
  (require (submod ".." repl))
  (driver-loop))
