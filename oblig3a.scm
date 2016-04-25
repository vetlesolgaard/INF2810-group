(load "prekode3a.scm")
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (display "computing fib of ")
             (display n) (newline)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;;1a)
#|(define mem
  (lambda (mess f)
      (if(eq? mess 'memoize)
          (let ((table (make-table)))
            (lambda args
              (if (not(pair? args))
                  f
                  (let ((prev-result (lookup args table)))
                    (or prev-result
                        (let ((result (apply f args)))
                          (insert! args result table)
                          result))))))
          
          (f))
          
      ))|#


(define (mem mess f)
  (cond ((eq? mess 'memoize)
         (let ((table (make-table)))
           (lambda args
             (display args)(newline)
             (cond ((and (not(null? args)) (eq? (car args) 'unmemoize)) f)
                   (else
                    (let ((prev-result (lookup args table)))
                      (or prev-result
                          (let ((result (apply f args)))
                            (insert! args result table)
                            result))))))))
        
        ((eq? mess 'unmemoize) (f 'unmemoize))
        (else (display "unknown command")))
  )
#|

(set! fib (mem 'memoize fib))
(fib 3)
(fib 5)
(set! fib (mem 'unmemoize fib))
(fib 3)
(fib 5)

(procedure? fib)

(procedure? (test-proc)) ;;#f
(set! test-proc (mem 'memoize test-proc))
(test-proc 40 41 42 43 44)
|#


;;1c)
;;Problemet her er at fib peker ikke tilbake på mem-fib, den peker til fibutregningen,
;;og vil bare ha tilgang til å sjekke tabellen 1 gang hver gang vi kaller mem-fib.
;;kall 1 (mem-fib 3) hele utregningen
;;kall 2 (mem-fib 3) finner tallet i tabellen
;;kall 3 (mem-fib 4) hele utregningen

;;1d)
(define get-value-from-name
  (lambda (name arguments)
    (cond ((null? arguments) #f)
          ((eq? name (car arguments)) (cadr arguments))
          (else (get-value-from-name name (cddr arguments))))))

(define greet
  (lambda args
    (let ((time  (or (get-value-from-name 'time args) "day"))
          (title (or (get-value-from-name 'title args) "friend")))
      (display "Good ")
      (display time)
      (display " ")
      (display title)
      (display "."))))


(greet 'time "morning")

;;2a)


