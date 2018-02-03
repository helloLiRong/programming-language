;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst)
             (racketlist->mupllist (cdr lst)))))

;(define a (racketlist->mupllist (list 1 2 3 4)))
;a
(define (mupllist->racketlist slst)
  (if (aunit? slst)
      empty
      (cons (apair-e1 slst)
            (mupllist->racketlist (apair-e2 slst)))))
;(mupllist->racketlist a)


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  ;(display (format "dump env:~a~n" env) )
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(or (int? e)
             (closure? e)
             (aunit? e))
         e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2)
                    (> (int-num v1) (int-num v2)))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env)))]
        [(mlet? e) (eval-under-env
                    (mlet-body e)
                    (cons (cons (mlet-var e)
                                (eval-under-env (mlet-e e) env))
                          env))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let ([func-name  (fun-nameopt (closure-fun v1))]
                     [func-param (fun-formal  (closure-fun v1))]
                     [func-body  (fun-body    (closure-fun v1))]
                     [func-env (closure-env v1)])
                 (eval-under-env func-body (if func-name
                                               (cons (cons func-name  v1)
                                                     (cons (cons func-param v2)
                                                           func-env))
                                               (cons (cons func-param v2)
                                                     func-env))))        
               (error "MUPL call applied to non-closure:~v" v1)))]
               
        [(apair? e) (apair (eval-under-env (apair-e1 e) env)
                           (eval-under-env (apair-e2 e) env))]
        [(fst? e) (apair-e1 (eval-under-env (fst-e e) env))]
        [(snd? e) (apair-e2 (eval-under-env (snd-e e) env))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env))
                          (int 1)
                          (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;(display "-----------------------test problem 2 case ------------\n")
;(eval-exp (add (int 1) (int 2)))

;(define body  (mlet (var "a9")
 ;                   (int 3)
  ;                  (add (var "a1") (var "b"))))

;(define func (fun "a" "b" body))

;(eval-exp func)

;(define func-with-closure (mlet (var "a1")
;                                (int 16)
 ;                               func))

;(eval-exp func-with-closure)
;(eval-exp (call func-with-closure (int 100)))


;(eval-exp ))
;; Problem 3

;(display "-----------------------test problem 3 case ------------\n")

(define (ifaunit e1 e2 e3)
  (ifeq (isaunit e1)
        (int 0)
        e3
        e2))

(define (mlet*-1 lstlst e2)
  (if (null? lstlst)
      e2
      (mlet* (cdr lstlst)
             (mlet (car (car lstlst))
                   (cdr (car lstlst))
                   e2))))
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst))
            (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))
      
(define (ifeq-1 e1 e2 e3 e4)
  (let ([v1 (eval-exp e1)]
        [v2 (eval-exp e2)])
    (if (and (int? v1)
             (int? v2)
             (= (int-num v1) (int-num v2)))
        (eval-exp e3)
        (eval-exp e4))))

      
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

;; Problem 4
;(define mupl-map "CHANGE")
(define mupl-map (fun "map" "fun"
                      (fun #f "list"
                           (mlet "l" (var "list")
                                 (ifaunit (var "l")
                                          (aunit)
                                          (apair (call (var "fun") (fst (var "l")))
                                                 (call (call (var "map") (var "fun")) (snd (var "l")))))))))

;"CHANGE (notice map is now in MUPL scope)"
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "int"
             (call (var "map")
                   (fun #f "x" (add (var "x") (var "int")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
