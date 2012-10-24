#lang racket

(define-syntax (cps e)
  (syntax-case e (with rec lam cnd seq set quote display read-number)
    [(_ (generator (yield) (v) b))
     (and (identifier? #'v) (identifier? #'yield))
     #'(λ (k)
         (k (let ([where-to-go (λ (v) (error 'where-to-go "use before init"))])
              (letrec ([resumer (λ (v)
                                  ((cps b) 
                                   (λ (k) 
                                     (error 'resumer "generator done"))))]
                       [yield (λ (v gen-k)
                                (begin
                                  (set! resumer gen-k)
                                  (where-to-go v)))])
                (λ (a dyn-k)
                  (begin
                    (set! where-to-go dyn-k)
                    (resumer a)))))))]
    [(_ (with (v e) b))
     #'(cps ((lam (v) b) e))]
    [(_ (rec (v f) b))
     #'(cps (with (v (lam (k) (error 'dummy "nothing")))
                (seq
                  (set v f)
                  b)))]
    [(_ (lam (a) b))
     (identifier? #'a)
     #'(lambda (k)
         (k (lambda (a dyn-k)
              ((cps b) dyn-k))))]
    [(_ (cnd tst thn els))
     #'(lambda (k)
         ((cps tst) (lambda (tstv)
                      (if tstv
                          ((cps thn) k)
                          ((cps els) k)))))]
    [(_ (seq e1 e2))
     #'(lambda (k)
         ((cps e1) (lambda (_)
                     ((cps e2) k))))]
    [(_ (set v e))
     #'(lambda (k)
         ((cps e) (lambda (ev)
                    (k (set! v ev)))))]
    [(_ 'e)
     #'(lambda (k) (k 'e))]
    [(_ (f a))
     #'(lambda (k)
         ((cps f) (lambda (fv)
                    ((cps a) (lambda (av)
                               (fv av k))))))]
    [(_ (f a b))
     #'(lambda (k)
         ((cps a) (lambda (av)
                    ((cps b) (lambda (bv)
                               (k (f av bv)))))))]
    [(_ atomic)
     #'(lambda (k) (k atomic))]))

; ----------------------------------------------------------------

(define (run c) (c identity))

(run (cps (with (ns (generator (yield) (from)
                                 (rec (f (lam (n)
                                              (seq
                                               (yield n)
                                               (f (+ n 1)))))
                                   (f from))))
                  (seq
                   (ns 0)
                   (seq
                    (ns 0)
                    (ns 0))))))

(run (cps (with (ns (generator (yield) (from)
                                 (rec (f (lam (n)
                                              (yield n)))
                                   (f from))))
                  (seq 
                   (ns 0)
                   (ns 0)))))
