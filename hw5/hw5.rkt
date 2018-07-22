;; Programming Languages, Homework 5
;; author Dingze Wang

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

(define (racketlist->mupllist rlist)
  (if (null? rlist)
      (aunit)
      (apair (car rlist) (racketlist->mupllist (cdr rlist)))))


(define (mupllist->racketlist mlist)
  (if (aunit? mlist)
      null
      (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))))
  

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
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
        [(int? e) e]
        [(aunit? e) (aunit)]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        ;; if e1 > e2, then e3 else e4
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL addition applied to non-number")))]
        ;; fun
        [(fun? e) (closure env e)]
        ;; closure
        [(closure? e) e]
        ;; bind a value to a var and eval the expression with inside env
        [(mlet? e)
         (let ([env (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env)])
           (eval-under-env (mlet-body e) env))]
        ;; eval apair's two element and cons a new apair
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        ;; first element of a apair
        [(fst? e)
         (let ([result (eval-under-env (fst-e e) env)])
           (if (apair? result)
           (apair-e1 result)
           (error "MUPL first element of pair applied to non-pair")))]
        ;; second element of a apair
        [(snd? e)
         (let ([result (eval-under-env (snd-e e) env)])
           (if (apair? result)
           (apair-e2 result)
           (error "MUPL second element of pair applied to non-pair")))]
        ;; isaunit
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        
        ;; A call evaluates its first and second subexpressions to values. If the first is not a closure, it is an
        ;; error. Else, it evaluates the closure’s function’s body in the closure’s environment extended to map
        ;; the function’s name to the closure (unless the name field is #f) and the function’s argument-name
        ;; (i.e., the parameter name) to the result of the second subexpression.
        ;; (struct closure (env fun) #:transparent)
        ;; (struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([clo-env (closure-env v1)]
                      [clo-fun (closure-fun v1)]
                      [env-first (cons (cons (fun-formal clo-fun) v2) clo-env)]
                      [env-final (if (fun-nameopt clo-fun)
                                    (cons (cons (fun-nameopt clo-fun) v1) env-first)
                                    env-first)])
                 (eval-under-env (fun-body clo-fun) env-final))
               (error (format "bad MUPL expression, not a closure for first argument: ~v" e))))]
        
               
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
;; (struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "x" e1) (cons "y" e2))
         (ifgreater (var "x") (var "y") e4 (ifgreater (var "y") (var "x") e4 e3))))
    

;; Problem 4

(define mupl-map
  (fun #f "function"
       (fun "mapfunc" "mupllst"
            (ifaunit (var "mupllst")
                     (aunit)
                     (apair (call (var "function") (fst (var "mupllst")))
                            (call (var "mapfunc") (snd (var "mupllst"))))))))


(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "increment"
             (call (var "map")
                   (fun #f "curnum" (add (var "increment")
                                         (var "curnum")))))))


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
