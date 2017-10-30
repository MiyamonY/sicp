;;;
;; File:  ex4_2.rkt
;; Author: ymiyamoto
;;
;; Created on Mon Oct 30 20:36:02 2017
;;
#lang racket

;; a. eval treats (define x 3) as function call

;; b.
(module+ test
  (require rackunit)
  (require rackunit/text-ui))

(module util racket
  (provide tagged-list? underlying-apply)

  (define underlying-apply apply)

  (define (tagged-list? exp tag)
    (cond ((pair? exp) (eq? (car exp) tag))
          ((mpair? exp) (eq? (mcar exp) tag)))))

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

  (define (application? exp) (tagged-list? exp 'call))
  (define (operator exp) (cadr exp))
  (define (operands exp) (cddr exp))

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

  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp exps) (car exps))
  (define (rest-exps exps) (cdr exps)))

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
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
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
          (else
           (error "Unknown expression type -- EVAL" exp)))))

(module+ test
  (require (submod ".." env))
  (require (submod ".." eval))

  (define test-env (setup-environment))

  (run-tests
   (test-suite
    "test env"
    (test-eqv? "function application is defined by call tag"
               (eval '(begin
                        (define sum (lambda (x y) (call
                                                   + x y)))
                        (call sum 1 2))
                     test-env) 3))))
