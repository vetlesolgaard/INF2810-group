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
(define (mem message proc)
  (cond ((eq? message 'memoize)
         (let ((table (make-table)))
           (lambda args
             (display args)(newline)
             (cond ((and (not(null? args)) (eq? (car args) 'unmemoize)) proc)
                   (else
                    (let ((prev-result (lookup args table)))
                      (or prev-result
                          (let ((result (apply proc args)))
                            (insert! args result table)
                            result))))))))
        
        ((eq? message 'unmemoize) (proc 'unmemoize))
        (else (display "unknown command"))))

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

;;2a)
(define list '(1 2 3 4 5))

(define list-to-stream
  (lambda (args)
    (if (null? args)
        the-empty-stream
        (cons-stream (car args) (list-to-stream (cdr args))))))

(define stream-to-list
  (lambda (stream . count)
    (cond ((stream-null? stream) the-empty-stream)
          ((null? count) (cons
                          (stream-car stream)
                          (stream-to-list (stream-cdr stream))))          
          ((zero? (car count)) the-empty-stream)
          (else (cons
                 (stream-car stream)
                 (stream-to-list (stream-cdr stream) (- (car count) 1)))))))
;;2b)
(define empty-streams?
  (lambda (args)
    (if(stream-null? (cdr args))
       (stream-null? (car args))
       (or (stream-null? (car args)) (empty-streams? (cdr args))))))

(define (stream-map proc . argstreams)
  (if (empty-streams? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


;;2c) Du vil få et stort problem med uendelige strømmer.
;;Den vil aldri stoppe å søke etter duplikater.

;;2d)
(define seen '())
(define seen-it-before?
  (let ((seen '()))
    (lambda (item)
      (if (not (member item seen))
          (begin (set! seen (cons item seen)) #t)
          #f))))
           
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (begin (seen-it-before? (stream-car stream)) (stream-car stream))
                   (stream-filter seen-it-before? (stream-cdr stream)))))

(define a (remove-duplicates (list-to-stream '(1 1 1 2 4 3 3 4 2 1 1 2 3 3 2))))
(stream-ref a 1)
(stream-ref a 3)

;;2e) Hver gang vi kaller på x, (stream-cdr x) og videre ned strømmen, vil (apply proc....)
;; hele tiden utføre sin oppgave som er å kjøre show. Show returnerer verdien til x, og som
;; erstatter lenken fra cons-cellen som tidligere gikk til (apply proc ....)

;;2f)
(define (mull-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream (stream-car nats)
              (mull-streams nats factorials)))
