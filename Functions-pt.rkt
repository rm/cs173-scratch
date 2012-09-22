#lang plai-typed

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [appC (f a) (local ([define f-value (interp f env)])
                  (interp (closV-body f-value)
                          (extend-env (bind (closV-arg f-value)
                                            (interp a env))
                                      (closV-env f-value))))]
    [plusC (l r) (num+ (interp l env)
                       (interp r env))]
    [multC (l r) (num* (interp l env)
                       (interp r env))]
    [lamC (a b) (closV a b env)]))

(define (num+ [l : Value] [r : Value]) : Value
  (cond 
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond 
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))

; --

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [s : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "unbound symbol")]
    [(symbol=? s (bind-name (first env))) (bind-val (first env))]
    [else (lookup s (rest env))]))

; Tests
(print-only-errors #t)
(test (interp (appC (lamC 'x (plusC (idC 'x) (idC 'x))) (numC 3)) 
              mt-env)
      (numV 6))
(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))
(test (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                   (numC 4)))
                    (numC 3))
              mt-env)
      (numV 7))
