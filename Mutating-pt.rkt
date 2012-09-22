#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (l : Location)])

(define-type Result
  [v*s (v : Value) (s : Store)])

(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define (interp (expr : ExprC) (env : Env) (sto : Store)) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [idC (s) (v*s (fetch (lookup s env) sto) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                  [v*s (v-f s-f)
                       (type-case Result (interp a env s-f)
                         [v*s (v-a s-a)
                              (let ([where (new-loc)])
                                (interp (closV-body v-f)
                                        (extend-env (bind (closV-arg v-f)
                                                          where)
                                                    (closV-env v-f))
                                        (override-store (cell where v-a)
                                                        s-a)))])])]
    [plusC (l r)
           (type-case Result (interp l env sto)
             [v*s (v-l s-l)
                  (type-case Result (interp r env s-l)
                    [v*s (v-r s-r)
                         (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r)
           (type-case Result (interp l env sto)
             [v*s (v-l s-l)
                  (type-case Result (interp r env s-l)
                    [v*s (v-r s-r)
                         (v*s (num* v-l v-r) s-r)])])]
    [lamC (a b) (v*s (closV a b env) sto)]
    [boxC (a)
          (type-case Result (interp a env sto)
            [v*s (v-a s-a)
                 (let ([where (new-loc)])
                   (v*s (boxV where)
                        (override-store (cell where v-a)
                                        s-a)))])]
    [unboxC (a)
            (type-case Result (interp a env sto)
              [v*s (v-a s-a)
                   (v*s (fetch (boxV-l v-a) s-a) s-a)])]
    [setboxC (b v) (type-case Result (interp b env sto)
                     [v*s (v-b s-b)
                          (type-case Result (interp v env s-b)
                            [v*s (v-v s-v)
                                 (v*s v-v
                                      (override-store 
                                       (cell (boxV-l v-b)
                                             v-v)
                                       s-v))])])]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1) 
                         (interp b2 env s-b1)])]))

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

(define (lookup [for : symbol] [env : Env]) : Location
  (cond 
    [(empty? env) (error 'lookup "unbound symbol")]
    [(symbol=? for (bind-name (first env))) (bind-val (first env))]
    [else (lookup for (rest env))]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "location not found")]
    [(= loc (cell-location (first sto))) (cell-val (first sto))]
    [else (fetch loc (rest sto))]))

(define new-loc
  (let ([n (box 0)])
    (lambda () (begin
                 (set-box! n (add1 (unbox n)))
                 (unbox n)))))


; Tests
(print-only-errors #t)
(test (v*s-v 
       (interp (appC (lamC 'x (plusC (idC 'x) (idC 'x))) (numC 3)) 
               mt-env mt-store))
      (numV 6))
(test (v*s-v 
       (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
               mt-env mt-store))
      (numV 15))
(test (v*s-v 
       (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                    (numC 4)))
                     (numC 3))
               mt-env mt-store))
      (numV 7))
(test (v*s-v 
       (interp 
        (appC 
         (lamC 'b
               (seqC (seqC (setboxC (idC 'b) (plusC (numC 1)
                                                    (unboxC (idC 'b))))
                           (setboxC (idC 'b) (plusC (numC 1)
                                                    (unboxC (idC 'b)))))
                     (unboxC (idC 'b))))
         (boxC (numC 0)))
        mt-env mt-store))
      (numV 2))
