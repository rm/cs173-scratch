#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse s) : ArithC
  (cond 
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]))]
    [else (error 'parse "invalid list input")]))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

; tests
(print-only-errors #t)

(test (interp (parse '(+ 1 2))) 3)
(test (interp (parse '(* 3 4))) 12)
(test (interp (parse '(* (+ 1 2)
                         (* (+ 3 4)
                            (+ 5 6)))))
      231)
