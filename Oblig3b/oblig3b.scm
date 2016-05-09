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

(set! the-global-environment (setup-environment))
(primitive-procedure-objects)
(install-primitives! 'apekatt (lambda (x) (* x x)))
(primitive-procedure-objects)


the-global-environment

(mc-eval '(apekatt 4) the-global-environment)
(read-eval-print-loop)

