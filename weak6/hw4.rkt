
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;;problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;;problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;;problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([len (length xs)])
              (car (list-tail xs (remainder n len))))]))

;;problem 4
(define (stream-for-n-steps stream n)
  (letrec ([f (lambda (step cur-s)
                (let ([pr (cur-s)])
                  (if (= step n)
                  null
                  (cons (car pr) (f (+ step 1) (cdr pr))))))])
    (f 0 stream)))

;;problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons
                 (if (= (remainder x 5) 0) (- x) x)
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;;problem 6
(define dan-then-dog
  (letrec ([dan-fun (lambda () (cons "dan.jpg" dog-fun))]
           [dog-fun (lambda () (cons "dog.jpg" dan-fun))])
    dan-fun))

;;problem 7
(define (stream-add-zero stream)
  (let ([pr (stream)])
    (lambda ()
      (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))

;;problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
    (lambda() (f 0))))

;;problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (position)
                (if (= position (vector-length vec)) #f
                    (let ([v-of-vec (vector-ref vec position)])
                      (if (and (pair? v-of-vec) (equal? v (car v-of-vec)))
                          v-of-vec
                          (f (+ position 1))))))])
    (f 0)))

;;problem 10
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [position -1])
   (lambda (v)
    (let ([cache-value (vector-assoc v cache)])
      (if cache-value cache-value
          (let ([real-value (assoc v xs)])
            (if real-value
                (begin (set! position (if (= position (sub1 n)) 0 (add1 position)))
                       (vector-set! cache position real-value)                      
                       real-value)
                #f)))))))
                       
        