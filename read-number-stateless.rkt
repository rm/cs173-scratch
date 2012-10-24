#lang plai-typed

(define-type-alias label number)
(define new-label
  (let ([l 0])
    (lambda () : label
      (begin
        (set! l (add1 l))
        l))))

(define table (make-hash empty))

(define (read-number/stateless [prompt : string] rest)
  (let ([g (new-label)])
    (begin
      (hash-set! table g rest)
      (display prompt)
      (display " (to enter it, use the label `")
      (display g)
      (display "')\n")
      (error 'read-number/stateless "halting"))))

(define (resume [g : label] [n : number])
  ((some-v (hash-ref table g)) n))

(define (read-number [prompt : string])
  (begin
    (display prompt)
    0))

;(read-number/suspend "First number"
;                     (lambda (v1)
;                       (read-number/suspend "Second number"
;                                            (lambda (v2)
;                                              (display
;                                               (+ v1 v2))))))

(define cookie 'bazinga)

(define (prog1 v1)
  (begin
    (set! cookie v1)
    (read-number/stateless "Second number" prog2)))
 
(define (prog2 v2)
  (display (+ cookie v2)))

(read-number/stateless "First number" prog1)
 