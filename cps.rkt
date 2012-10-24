#lang racket

(define-syntax (cps e)
  (syntax-case e (with rec lam cnd seq set quote display read-number)
    [(_ (with (v e) b)) ; <cps-macro-with-case>
     #'(cps #'((lam (v) b) e))]
    [(_ (rec (v f) b)) ; <cps-macro-rec-case>
     #'(cps #'(with (v (lam (k) (error 'dummy "nothing")))
                (seq
                  (set v f)
                  b)))]
    [(_ (lam (a) b)) ; <cps-macro-lam-case>
     (identifier? #'a)
     #'(lambda (k)
         (k (lambda (a dyn-k)
              ((cps b) dyn-k))))]
    [(_ (cnd tst thn els)) ; <cps-macro-cnd-case>
     #'(lambda (k)
         ((cps tst) (lambda (tstv)
                      (if (tstv)
                          ((cps thn) k)
                          ((cps els) k)))))]
    [(_ (seq e1 e2)) ; <cps-macro-seq-case>
     #'(lambda (k)
         ((cps e1) (lambda (_)
                     ((cps e2) k))))]
    [(_ (set v e)) ; <cps-macro-set-case>
     #'(lambda (k)
         ((cps e) (lambda (ev)
                    (k (set! v ev)))))]
    [(_ 'e) ; <cps-macro-quote-case>
     #'(lambda (k) (k 'e))]
    [(_ (display output)) ; <cps-macro-display-case>
     #'(lambda (k)
         ((cps output) (lambda (ov)
                         (k (display ov)))))]
    [(_ (read-number prompt)) ; <cps-macro-read-number-case>
     #'(lambda (k)
         ((cps prompt) (lambda (pv)
                         (read-number/suspend pv k))))]
    [(_ (f a)) ; <cps-macro-app-1-case>
     #'(lambda (k)
         ((cps f) (lambda (fv)
                    ((cps a) (lambda (av)
                               (fv av k))))))]
    [(_ (f a b)) ; <cps-macro-app-2-case>
     #'(lambda (k)
         ((cps a) (lambda (av)
                    ((cps b) (lambda (bv)
                               (k (f av bv)))))))]
    [(_ atomic) ; <cps-macro-atomic-case>
     #'(lambda (k) (k atomic))]))

; ----------------------------------------------------------------

; (define-type-alias label number)
(define new-label
  (let ([l 0])
    (lambda ()
      (begin
        (set! l (add1 l))
        l))))

(define table (make-hash empty))

(define (read-number/suspend prompt rest)
  (let ([g (new-label)])
    (begin
      (hash-set! table g rest)
      (display prompt)
      (display " (to enter it, use the label `")
      (display g)
      (display "')\n")
      (error 'read-number/suspend "halting"))))

(define (resume g n)
  ((hash-ref table g) n))

;(read-number/suspend "First number"
;                     (lambda (v1)
;                       (read-number/suspend "Second number"
;                                            (lambda (v2)
;                                              (display
;                                               (+ v1 v2))))))

; ----------------------------------------------------------------

;(cps (+ (read-number "First number")
;        (read-number "Second number")))

(define (run c) (c identity))
;(test (run (cps 3)) 3)
;(test (run (cps ((lam () 5)))) 5)
;(test (run (cps ((lam (x) (* x x)) 5))) 25)
;(test (run (cps (+ 5 ((lam (x) (* x x)) 5)))) 30)

;(run (cps (display (+ (read-number "First")
;                      (read-number "Second")))))

(run (cps ((lam (x) x) 3)))