#lang plai

;(define-syntax my-let-1
;  (syntax-rules ()
;    ([my-let-1 (var val) body]
;     ((lambda (var) body) val))))
;
;;(my-let-1 (x 5) (+ x x))
;
;(define-syntax my-let-2
;  (syntax-rules ()
;    ([my-let-2 ([var val] ...) body]
;     [(lambda (var ...) body) val ...])))
;
;;(my-let-2 [(x 5)] (+ x x))
;
;(define-syntax foo
;  (syntax-rules ()
;    [(foo val ...) (+ val ...)]))
;     
;(define-syntax bar
;  (syntax-rules ()
;    [(bar val ...) (foo 7 val ...)]))
;
;;(bar 1 2 3 4 5 6)
;
;(define-syntax (my-let-3 x)
;  (syntax-case x ()
;    [(my-let-3 (var val) body)
;     #'((lambda (var) body) val)]))
;
;;(my-let-3 (x 2) (+ x x))

;(define-syntax (my-syntax-rules x)
;  (syntax-case x ()
;    [(my-syntax-rules _ [(in out)])
;     #'(syntax-case in _
;         [in out])]))
;
;(define-syntax (my-let-4 x)
;  (my-syntax-rules
;    ([my-let-4 (var val) body]
;     [(lambda (var) body) val])))
;
;(my-let-4 (x 3) (* x x))

(define-syntax (my-let-5 x)
  (syntax-case x ()
    [(my-let-5 (var val) body)
     (identifier? #'var)
     #'([lambda (var) body] val)]))

;(my-let-5 (x 2) (+ 1 x))
;(my-let-5 (1 2) (+ 1 x))

(define-syntax (my-or-2 x)
  (syntax-case x ()
    [(my-or-2)
     #'#f]
    [(my-or-2 e0 e1 ...)
     #'(if e0
           e0
           (my-or-2 e1 ...))]))

;(my-or-2 #f #t)

(define-syntax (my-or-3 x)
  (syntax-case x ()
;    [(my-or-3)
;     #'#f]
    [(my-or-3 e)
     #'e]
    [(my-or-3 e0 e1 ...)
     #'(if e0
           e0
           (my-or-2 e1 ...))]))

(my-or-3 #f #t)
