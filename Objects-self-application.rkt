#lang plai

; self-application, not recursive define

(define (msg obj message . args)
  (apply (obj obj message) args))

(define (mt)
  (λ (self m)
    (case m
      [(add) (λ () 0)])))

(define (node value left right)
  (λ (self m)
    (case m
      [(add) (λ () (+ value
                      (msg left 'add)
                      (msg right 'add)))])))

(define (mt/size)
  (let ([parent (mt)])
    (λ (self m)
      (case m
        [(size) (lambda () 0)]
        [else (parent self m)]))))

(define (node/size value left right)
  (let ([parent (node value left right)])
    (λ (self m)
      (case m
        [(size) (λ () (+ 1
                         (msg left 'size)
                         (msg right 'size)))]
        [else (parent self m)]))))

; tests
(define a-tree/size
  (node/size 10
             (node/size 5 (mt/size) (mt/size))
             (node/size 15 
                        (node/size 6 (mt/size) (mt/size))
                        (mt/size))))
(test (msg a-tree/size 'add) (+ 10 5 15 6))
(test (msg a-tree/size 'size) 4)
