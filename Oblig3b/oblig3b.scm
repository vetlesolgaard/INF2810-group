(load "evaluator.scm")
(set! the-global-environment (setup-environment))
(mc-eval '(+ 1 2) the-global-environment)
(mc-eval '(define foo (lambda (x) (* x x))) the-global-environment)
;;(read-eval-print-loop)


;;1a) Hver gang vi lager et nytt kall på en funksjon, (exp), blir exp sent inn til mc-eval
;; og der blir det utført noen tester for å se om kommandoen finnes i evaluatoren.
;; Der er det en test for special-form som igjen tester for f.eks cond.
;; Når (car exp) er cond, vil det neste være et kall på eval-special-form som utfører
;; funksjonen cond som den skal utføres, og bindingen til tallet som blir sendt inn i foo,
;; blir ignorert.

;;2a)
;;
(define (install-primitives! proc func)
  (define-variable! proc (list 'primitive func) the-global-environment)
  );;install primitives = 2b)

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
;;      her kan vi legge til flere primitiver.
        (list '1+ (lambda (x) (+ x 1)))
        (list '1- (lambda (x) (- x 1)))
        (list '< <)
        (list '> >)
           (list 'install-primitives! install-primitives! )
        ))

;;2b)


;;3a)

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-new-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and (cdr exp) env)) ;; NEW
        ((or? exp) (eval-or (cdr exp) env));;NEW
        ((let? exp) (let->lambda exp env));;NEW
        ((while? exp) (eval-while exp env))));;NEW

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; NEW
        ((or? exp) #t) ;; NEW
        ((let? exp) #t) ;;NEW
        ((while? exp) #t);; NEW
        (else #f)))

(define (and? exp) (tagged-list? exp 'and)) ;; NEW

(define (eval-and exp env) ;; NEW
  (let ((last-true (mc-eval (car exp) env)))
    (cond ((not (true? value)) #f)
          ((null? (cdr exp)) last-true)
          (else (set! last-true value)(eval-and (cdr exp) env)))))

(define (or? exp) (tagged-list? exp 'or)) ;; NEW

(define (eval-or exp env) ;; NEW
  (cond ((null? exp) #f)
        ((true? (mc-eval (car exp) env)) (mc-eval (car exp) env))
        (else (eval-or (cdr exp) env))))


(set! the-global-environment (setup-environment))
(primitive-procedure-objects)
(install-primitives! 'apekatt (lambda (x) (* x x)))
(primitive-procedure-objects)

;; Oppgave 3b)
(define (else? exp) ;; NEW
  (eq? (car exp) 'else))

(define (execute-then exp env) ;; NEW
  (if (eq? (caddr exp) 'then)
      (mc-eval (cadddr exp) env)
      (display "SYNTAX ERROR")))

(define (elsif-test exp env) ;; NEW
  (if (eq? (car exp) 'elsif)
      (mc-eval (cadr exp) env)
      (display "SYNTAX ERROR"))) 

(define (eval-new-if exp env) ;; NEW
  (define (parser exp env)
    (cond ((else? exp) (mc-eval (cadr exp) env))
          ((true? (elsif-test exp env)) (execute-then exp env))
          (else (parser (cddddr exp) env))))
  (if (true? (mc-eval (if-predicate exp) env))      
      (execute-then exp env)
      (parser (cddddr exp) env)))

;;3c)



(define (let? exp)
  
  (tagged-list? exp 'let))

(define (let-exp exp)
  (if (null? (cdr exp))
      (cons (cadar exp) '())
      (cons (cadar exp) (let-exp (cdr exp)))))
  
(define (let-var exp)
  (if (null? (cdr exp))
      (cons (caar exp) '())
      (cons (caar exp) (let-var (cdr exp)))))

(define (let->lambda exp env)
  (mc-eval (cons (list 'lambda (let-var (cadr exp)) (cons 'begin (cddr exp))) (let-exp (cadr exp))) env)
  )

;;3d)
(define (let-body exp)
   (cons 'begin (cddddr exp)))

(define (let->lambda exp env)
  (let ((vars '())
        (exps '())
        )
      (define (let->lambda-1 exp)
        (set! vars (cons (car exp) vars))
        (set! exps (cons (caddr exp) exps))
        (cond ((eq? 'and (cadddr exp))(let->lambda-1 (cddddr exp)))
              ((eq? 'in (cadddr exp))(mc-eval (cons (list 'lambda vars (let-body exp)) exps) env))))
  (let->lambda-1 (cdr exp))))
;;(let x = 2 and y = 3 in (+ x y))

;;3e)
(define (while? exp)
  (tagged-list? exp 'while))

(define (eval-while exp env)
  (let ((body (cons 'begin (cddr exp)))
        (pred (cadr exp)))
    
    (define (while)
      (if (mc-eval pred env)
          (begin (mc-eval body env) (while))
          'done))
   (while)))

(set! the-global-environment (setup-environment))

(mc-eval '(apekatt 4) the-global-environment)
(newline)(display "Oppgave 3b")(newline)
(mc-eval '(if (= 1 2) then 1 elsif (= 1 3) then 2 else 3) the-global-environment)
(mc-eval '(if (= 1 1) then 1 else 2) the-global-environment)

(read-eval-print-loop)



