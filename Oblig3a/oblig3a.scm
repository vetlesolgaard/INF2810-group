(load "prekode3a.scm")

;;1a)
(define (mem message proc)
  (cond ((eq? message 'memoize)
         (let ((table (make-table)))
           (lambda args
             
             (cond ((and (not(null? args)) (eq? (car args) 'unmemoize)) proc)
                   (else
                    (let ((prev-result (lookup args table)))
                      (or prev-result
                          (let ((result (apply proc args)))
                            (insert! args result table)
                            result))))))))
        
        ((eq? message 'unmemoize) (proc 'unmemoize))
        (else (display "unknown command"))))

#|(define (mem message proc)
    (let ((table (make-table)))
      (lambda args
        (cond ((and (eq? message 'memoize) (not(null? args)) (eq? (car args) 'unmemoize)) (display (car args))proc)
              ((eq? message 'memoize)
               (let ((prev-result (lookup args table)))
                 (or prev-result
                     (let ((result (apply proc args)))
                       (insert! args result table)
                       result))))
              
              ((eq? message 'unmemoize) (proc 'unmemoize))
              (else (display "unknown command"))))))|#
(display "1a) (set! fib (mem 'memoize fib)) og 4 fib kall med argumentene 3,3,2 og 4")
(newline)
(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 2)
(fib 4)
(newline)
(display "1b) (set! fib (mem 'unmemoize fib)) etterfulgt av 2 fib kall med argumentet 3")
(newline)
(set! fib (mem 'unmemoize fib))
(fib 3)
(fib 3)

(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)
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
      (display ".")(newline))))
(newline)
(display "1d)")
(display " greet 0 args")(newline)
(greet)
(display "greet 2 args: 'time evening")(newline)
(greet 'time "evening")
(display "greet 4 args: 'title sir 'time morning")(newline)
(greet 'title "sir" 'time "morning")
(display "greet 4 args: 'time afternoon 'title dear")(newline)
(greet 'time "afternoon" 'title "dear")

;;2a)
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
(newline)
(display "2a) Lager en test-list '(1 2 3 4 5)")(newline)
(define test-list '(1 2 3 4 5))
test-list
(display "list-to-stream test-list")(newline)
(define test-stream (list-to-stream test-list))
test-stream
(display "stream-cdr")(newline)
(stream-cdr test-stream)
(display "stream-to-list (stream-interval 10 20)")(newline)
(stream-to-list (stream-interval 10 20))
(display "show-stream nats 15")(newline)
(show-stream nats 15)
(display "stream-to-list nats 10")(newline)
(stream-to-list nats 10)
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
(newline)
(display "2b)")(newline)
(display "x = (stream-interval 10 20")(newline)
(define x (stream-interval 10 20))
(display "y = (list-to-stream '(1 2 3 4 6))") (newline)
(define y (list-to-stream '(1 2 3 4 6)))
(display "streampluss (stream-map + x y y)")(newline)
(define streampluss (stream-map + x y y))
(display "10 + 1 + 1")(newline)
streampluss
(display "11 + 2 + 2")(newline)
(stream-cdr streampluss)


;;2c) Du vil få et stort problem med uendelige strømmer.
;;Den vil aldri stoppe å søke etter duplikater.
;;evt feil rekkefølge også, men det er ikke et stort problem

;;2d)

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
(newline)
(display "2d)")(newline)
(display "duplicatelist '(1 1 1 2 4 3 3 4 2 1 1 2 3 3 2)")(newline)
(define duplicatelist '(1 1 1 2 4 3 3 4 2 1 1 2 3 3 2))
(display "a = (remove-duplicates (list-to-stream duplicatelist))")(newline)
(define a (remove-duplicates (list-to-stream duplicatelist)))
(display "stream-ref a 1")(newline)
(stream-ref a 1)
(display "stream-ref a 2")(newline)
(stream-ref a 2)


;;2e) Hver gang vi kaller på x, (stream-cdr x) og videre ned strømmen, vil (apply proc....)
;; hele tiden utføre sin oppgave som er å kjøre show. Show returnerer verdien til x, og som
;; erstatter lenken fra cons-cellen som tidligere gikk til (apply proc ....)

;;2f)
(define (mull-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream (stream-car nats)
              (mull-streams nats factorials)))
(newline)
(display "2f)")(newline)
(display "stream-ref factorials 5")
(stream-ref factorials 5)
