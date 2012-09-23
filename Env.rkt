#lang plai-typed

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (s) (lookup s env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          (extend-env (bind (fdC-arg fd)
                                            (interp a env fds))
                                      mt-env)
                          fds))]
    [plusC (l r) (+ (interp l env fds)
                    (interp r env fds))]
    [multC (l r) (* (interp l env fds)
                    (interp r env fds))]))

(define (lookup [s : symbol] [env : Env])
  (cond
    [(empty? env) (error 'lookup "unbound symbol")]
    [(symbol=? s (bind-name (first env))) (bind-val (first env))]
    [else (lookup s (rest env))]))

(define-type FunDefC
  [fdC (name : symbol)
       (arg : symbol)
       (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define (get-fundef [f : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond 
    ([empty? fds] (error 'get-fundef "missing function"))
    ([symbol=? f (fdC-name (first fds))] (first fds))
    (else (get-fundef f (rest fds)))))

; Tests
(print-only-errors #f)
(test (interp 
       (appC 'double (plusC (numC 1) (numC 2)))
       mt-env
       [list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))])
      6)

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)

(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)

(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)
