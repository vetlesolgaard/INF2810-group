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
#|(define primitive-procedures
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
        ))|#

;;2b)

(define (install-primitives! proc func)
  (define-variable! proc (list 'primitive func) the-global-environment)
  )

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
        (list 'install-primitives! install-primitives! )
        
        ))

;;3a)

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and (cdr exp) env)) ;; NEW
        ((or? exp) (eval-or (cdr exp) env)))) ;;NEW

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
        (else #f)))

(define (and? exp) (tagged-list? exp 'and)) ;; NEW

(define (eval-and exp env) ;; NEW
  (cond ((null? exp) #t)
        ((not (true? (mc-eval (car exp) env))) #f)
        (else (eval-and (cdr exp) env)))) 

(define (or? exp) (tagged-list? exp 'or)) ;; NEW

(define (eval-or exp env) ;; NEW
  (cond ((null? exp) #f)
        ((true? (mc-eval (car exp) env)) #t)
        (else (eval-or (cdr exp) env))))


(set! the-global-environment (setup-environment))
(primitive-procedure-objects)
(install-primitives! 'apekatt (lambda (x) (* x x)))
(primitive-procedure-objects)


the-global-environment

(mc-eval '(apekatt 4) the-global-environment)
(read-eval-print-loop)

