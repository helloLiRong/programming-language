
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride)) ; then clause
      null) ; else clause
  )

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")] ; number negative => raise error
    [(null? xs) (error "list-nth-mod: empty list")] ; list empty => raise error
    [else (let ([i (remainder n (length xs))]) ; in all other cases take i = n mod (xs.length)
            (car (list-tail xs i)))]) ; take the first element after you "remove" the first i elements from the list.
  )

(define (stream-for-n-steps s n)
  (if (> n 0)
      (let ([pair (s)]) ; stream is a thunk and i get the first pair out
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1)))); create the list recursively
      null) ; else return the empty list.
  )

(define funny-number-stream
  (letrec ([f (lambda (x) ; define a function f
                (cons (if (= (remainder x 5) 0) (- x) x) ; creates a pair with the first argument x or -x
                      (lambda () (f (+ x 1)))))]) ; and second argument an anonymous function that calls f(x+1)
          (lambda () (f 1)))) ; after defining f, funny-number-stream is set to be an anonymous function that when called, calls f(1)

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))]) ; end of definitions
    (lambda () (dan)))) ; dan-then-dog is set to be an anonymous function that when called calls dan


(define (stream-add-zero s)
  (letrec ([f (lambda (xs) ; function f takes input a stream
                (let ([pair (xs)]) ; take out the pair of the stream
                  (cons (cons 0 (car pair)) (lambda () (f (cdr pair)))); construct a pair from 0.car(stream) and replace the previous thunk with a new thunk that calls f
                  )
                )
              ]) ; end defining f
    (lambda () (f s)) ; now I am defining the function stream-add-zero as a function that when called applies f to the stream
    ) ; end letrec
  )

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))); created the stream
                ); defined anonymous function
              ]; finished defining f
           ); finished environement definition of let
    (lambda () (f 0)); this is the body of cycle-lists
    )
  )

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (= n (vector-length vec))
                    #f
                    (let ([p (vector-ref vec n)])
                      (if (and (pair? p) (equal? (car p) v))
                           p
                          (f (+ n 1))) ; endif
                      ); end let
                    ); end body
                ) ; end of f definition
              ]
           ); end of letrec
    (f 0); run f(0) in the letrec scope
    ); letrec end
  )

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [pos 0])
    (lambda (v)
      (or (vector-assoc v cache) ; if this is true the following will not be evaluated
          (let ([answer (assoc v list)])
            (and answer ; check that we actually found something in the list
                 (begin
                   (vector-set! cache pos answer)
                   (set! pos (if (= (+ pos 1) n) 0 (+ pos 1)))
                   answer)))))
    )  ; return the anonumous lambda function
  )




(define-syntax while-less
  (syntax-rules (do)
    ((while-less x do y)
      (let ([z x]) ; evaluate x once
        (letrec ([loop (lambda ()
                         (let ([w y]) ; evaluate y once per loop
                           (if (or (not (number? w)) (>= w z))
                               #t
                               (loop))))])
          (loop))))))