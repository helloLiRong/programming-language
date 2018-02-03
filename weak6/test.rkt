#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
;; put your code below
;; 1  number number number


(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next (s)])
            (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

(define funny-number-stream 
  (letrec ([f (lambda (n) (cons (if (= (remainder n 5) 0) (- n) n)
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))


(define (stream-for-n-steps1 s n)
  (let ([pair-value (s)])
    (if (= 0 n)
        '()
        (cons (car pair-value) (stream-for-n-steps1 (cdr pair-value) (- n 1 ))))))

;(stream-for-n-steps ones 10)
;(stream-for-n-steps ziranshu-stream 10)


;5
(define (funny-number-stream1)
  (letrec ([f (lambda (x)
                (cons (if (= 0 (remainder x 5))
                          (- x)
                          x)
                      (lambda () (f ( + x 1)))))])
    (f 1)))