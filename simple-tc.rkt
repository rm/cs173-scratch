#lang plai-typed

(define-type TyExprC
  [numC (n : number)]
  [boolC (b : boolean)]
  [idC (s : symbol)]
  [appC (fun : TyExprC) (arg : TyExprC)]
  [plusC (l : TyExprC) (r : TyExprC)]
  [multC (l : TyExprC) (r : TyExprC)]
  [lamC (arg : symbol) (argT : Type) (retT : Type) (body : TyExprC)]
  [ifC (cond : TyExprC) (then : TyExprC) (else : TyExprC)])

(define-type Type
  [numT]
  [boolT]
  [funT (arg : Type) (ret : Type)]
  [ifT (cond : Type) (then : Type) (else : Type)])

(define-type TyBinding
  [bind (id : symbol) (ty : Type)])
(define-type-alias TyEnv (listof TyBinding))
(define mt-ty-env (list))
(define extend-ty-env cons)

(define (lookup [name : symbol] [tenv : TyEnv]) : Type
  (cond 
    [(empty? tenv) (error 'lookup "name not found")]
    [(cons? tenv) (let [(f (first tenv))]
                    (cond 
                      [(equal? (bind-id f) name) (bind-ty f)]
                      [else (lookup name (rest tenv))]))]))

(define (tc [expr : TyExprC] [tenv : TyEnv]) : Type
  (type-case TyExprC expr
    [numC (n) (numT)]
    [boolC (b) (boolT)]
    [idC (s) (lookup s tenv)]
    [plusC (l r) (let ([lt (tc l tenv)]
                       [rt (tc r tenv)])
                   (if (and (numT? lt)
                            (numT? rt))
                       (numT)
                       (error 'tc "+ not both numbers")))]
    [multC (l r) (let ([lt (tc l tenv)]
                       [rt (tc r tenv)])
                   (if (and (numT? lt)
                            (numT? rt))
                       (numT)
                       (error 'tc "* not both numbers")))]
    [appC (f a) (let ([ft (tc f tenv)]
                      [at (tc a tenv)])
                  (if (and (funT? ft)
                           (equal? (funT-arg ft) at))
                      (funT-ret ft)
                      (error 'tc "type mismatch in function")))]
    [lamC (a argT retT b) 
          (if (equal? (tc b (extend-ty-env (bind a argT) tenv)) retT)
              (funT argT retT)
              (error 'tc "type mismatch in lambda"))]
    [ifC (c t e) (let ([ct (tc c tenv)]
                       [tt (tc t tenv)]
                       [et (tc e tenv)])
                   (cond [(not (equal? ct (boolT)))
                          (error 'tc "condition should be a boolean")]
                         [(not (equal? tt et))
                          (error 'tc "then and else must match")]
                         [else tt]))]))

; -- tests ------
(print-only-errors #t)

(test (tc (numC 3) mt-ty-env) (numT))
(test (tc (boolC true) mt-ty-env) (boolT))
(test (tc (boolC false) mt-ty-env) (boolT))
(test (tc (idC 'x) (extend-ty-env (bind 'x (numT)) mt-ty-env)) (numT))
(test (tc (idC 'y) 
          (extend-ty-env (bind 'x (numT)) 
                         (extend-ty-env (bind 'y (boolT))
                                        mt-ty-env)))
      (boolT))
(test/exn (tc (idC 'x) mt-ty-env) "name not found")
(test (tc (appC (lamC 'a (numT) (numT) (numC 1)) (numC 2)) mt-ty-env) (numT))
(test/exn (tc (appC (numC 1) (numC 2)) mt-ty-env) "type mismatch in function")
(test/exn (tc (appC (lamC 'a (numT) (numT) (numC 1)) (idC 'x)) mt-ty-env)
          "name not found")
(test/exn (tc (appC (lamC 'a (numT) (numT) (numC 1)) 
                    (lamC 'a (numT) (numT) (numC 1))) mt-ty-env)
          "type mismatch in function")
(test (tc (plusC (numC 1) (numC 2)) mt-ty-env) (numT))
(test (tc (multC (numC 1) (numC 2)) mt-ty-env) (numT))
(test/exn (tc (plusC (numC 1) (lamC 'a (numT) (numT) (numC 2))) mt-ty-env) 
          "+ not both numbers")
(test/exn (tc (multC (numC 1) (lamC 'a (numT) (numT) (numC 2))) mt-ty-env) 
          "* not both numbers")
(test (tc (lamC 'x (numT) (numT) (numC 1)) mt-ty-env) (funT (numT) (numT)))
(test/exn (tc (lamC 'c (numT) (numT) (lamC 'c (numT) (numT) (numC 1)))
              mt-ty-env)
          "type mismatch in lambda")
(test (tc (ifC (boolC true) (numC 1) (numC 2)) mt-ty-env) (numT))
(test/exn (tc (ifC (numC 1) (numC 2) (numC 3)) mt-ty-env)
          "condition should be a boolean")
(test/exn (tc (ifC (boolC false) (numC 1) (boolC true)) mt-ty-env)
          "then and else must match")
