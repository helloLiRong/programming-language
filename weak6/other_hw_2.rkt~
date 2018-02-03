
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
;; put your code below
;; 1  number number number


(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ stride low) high stride))))

;(if debug
;    (begin (sequence 3 11 2)
;           (sequence 3 8 3)
;           (sequence 3 2 1)) "")
;    

;; 2

(define (string-append-map xs suffix)
  (map (lambda (s)(string-append s suffix)) xs))


;(string-append-map (list "a" "b" "c" "d") "f")

;; 3 
(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs)  (error "list-nth-mod: empty list")]
        [else (car
               (list-tail xs
                          (remainder
                           n (length xs))))]))
;(list-nth-mod '() 1)
;(list-nth-mod (list 1 2 3 4 5) -1 )
;(list-nth-mod (list 1 2 3 4 5) 1 )
;(list-nth-mod (list 1 2 3 4 5) 5 )
;(list-nth-mod (list 1 2 3 4 5) 6 )

; 4
(define ones (lambda () (cons 1 ones)))

(define (ziranshu-stream)
  (letrec ([f (lambda(x)(cons x (lambda () (f (+ x 1)))))])
    (f 1)))


(define (stream-for-n-steps s n)
  (let ([pair-value (s)])
    (if (> 1 n)
        '()
        (cons (car pair-value) (stream-for-n-steps (cdr pair-value) (- n 1 ))))))

;(stream-for-n-steps ones 10)
;(stream-for-n-steps ziranshu-stream 10)


;5
(define (funny-number-stream)
  (letrec ([f (lambda (x)
                (cons (if (= 0 (remainder x 5))
                          (- x)
                          x)
                      (lambda () (f ( + x 1)))))])
    (f 1)))

;(stream-for-n-steps funny-number-stream 20)



;6
(define (dan-then-dog)
  (letrec ([f (lambda (name)
                (cons name
                      (lambda ()
                        (f (cond [(equal? "dan.jpg" name) "dog.jpg"]
                                 [#t "dan.jpg"])))))])
    (f "dan.jpg")))

;(define dan-then-dog (lambda (name)
;                       (cons name
;                             (dan-then-dog ))))

;(stream-for-n-steps dan-then-dog 20)

;7

(define (stream-add-zero s)
  (lambda ()
    (letrec ([f (lambda (x)
                  (cons (cons 0 (car x))
                        (lambda ()
                          (f ((cdr x))))))])
      (f (s)))))


;(stream-for-n-steps (lambda () (stream-add-zero dan-then-dog)) 20)



;8
(define (cycle-lists-older xs ys)
  (letrec ([f (lambda (xs ys)
                (cons (cons (car xs) (car ys))
                      (lambda ()
                        (f (append (cdr xs)
                                   (cons (car xs) null))
                           (append (cdr ys) (cons (car ys) null))))))])
    (f xs ys)))


; 6 lines.....
(define (cycle-lists xs ys)
  (lambda ()
    (define (incr n)
      (cons (cons (list-nth-mod xs n)
                  (list-nth-mod ys n))
            (lambda ()
              (incr (+ n 1)))))
    (incr 0)))

;(stream-for-n-steps (lambda () (cycle-lists (list 1 2 3) (list "a" "b" "c" "d"))) 20)
                     

;9
(define (vector-assoc-1 v vec)
  (letrec ([f (lambda (n)
                (if (= n (vector-length vec))
                    #f
                    (let ([t (vector-ref vec n)])
                      (if (pair? t)
                          (if (equal? v (car t))
                              t
                              (f (+ 1 n)))
                          (f (+ 1 n))))))])
          (f 0)))
(define (vector-assoc v vec)
  (letrec ([v-len (vector-length vec)]
           [f (lambda (n)
                (if (= n v-len)
                    #f
                    (let ([t (vector-ref vec n)])
                      (if (and (pair? t)
                               (equal? v (car t)))
                          t
                          (f (+ 1 n))))))])
    (f 0)))
;(vector-assoc 1 (vector 0 0 0))
;(vector-assoc 1 (vector 0 1 (list 1 2) 0 0))


; 10

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [pos 0])
    (lambda (v)
      (let ([r (vector-assoc v cache)])
        (if r
            r
            (let ([r (assoc v xs)])
              (if r
                  (begin
                    ; (display r)
                    ;(display pos)                   
                    (vector-set! cache pos r)
                    (display cache)
                    (set! pos (if (= pos (- n 1))
                                  0
                                  (+ pos 1)))
                    r)
                  #f)))))))

;(define ca (cached-assoc (list (cons 1 2) (cons 3 4)) 1))
;(ca 3)
;(ca 3)
;(ca 0)
;(ca 1)
;(ca 10)


(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([E1 e1])
       (letrec ([loop (lambda ()
                        (let ([E2 e2])
                          ;(display E2)
                          ;(display E1)
                          (if (or (not (number? E1))
                                  (not (number? E2))
                                  (<=  E1 E2))
                              #t
                              (loop))))])
         (loop)))]))



;(define a 2)
;(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
;a
;(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))

