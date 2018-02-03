
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      empty
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod : negative number")]
        [(empty? xs) (error "list-nth-mod : empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      empty
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec
      ([stream (lambda (n) (lambda ()(cons (if (= (remainder n 5) 0)
                                               (- n)
                                               n)
                                           (stream (+ 1 n)))))])                
  (stream 1)))

(define dan-then-dog
  (lambda () (cons
              "dan.jpg"
              (lambda () (cons "dog.jpg" dan-then-dog)))))

(define (stream-add-zero s)
  (lambda () (cons
              (cons 0 (car (s)))
              (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (letrec
      ([stream (lambda (n) (lambda ()(cons (cons (list-nth-mod xs n)
                                                 (list-nth-mod ys n))
                                           (stream (+ 1 n)))))])
    (stream 0)))

(define (vector-assoc v vec)
  (letrec
      ([loop (lambda (pos) (cond [(= pos (vector-length vec)) #f]
                                 [(and (pair? (vector-ref vec pos)) (equal? (car (vector-ref vec pos)) v)) (vector-ref vec pos)]
                                 [#t (loop (+ 1 pos))]))])
    (loop 0)))

(define (cached-assoc xs n)
  (let ([memo (make-vector n #f)]
        [pos 0])
    (lambda (v) (if (vector-assoc v memo)
                    (vector-assoc v memo)
                    (let ([ans (assoc v xs)])
                      (cond [ans (begin (vector-set! memo (remainder pos n) ans)
                                        (set! pos (+ 1 pos))
                                        ans)]
                             [#t ans]))))))