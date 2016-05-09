(load "evaluator.scm")
(set! the-global-environment (setup-environment))
(mc-eval '(+ 1 2) the-global-environment)
(define bar 40)
(read-eval-print-loop)


;;1a) Hver gang vi lager et nytt kall på en funksjon, (exp), blir exp sent inn til mc-eval
;; og der blir det utført noen tester for å se om kommandoen finnes i evaluatoren.
;; Der er det en test for special-form som igjen tester for f.eks cond.
;; Når (car exp) er cond, vil det neste være et kall på eval-special-form som utfører
;; funksjonen cond som den skal utføres, og bindingen til tallet som blir sendt inn i foo,
;; blir ignorert.

;;2a)  (list '1+ (lambda (x) (+ x 1)))
;;(list '1- (lambda (x) (- x 1)))

;;2b)
