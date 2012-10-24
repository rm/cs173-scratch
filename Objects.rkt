#lang plai

(define (msg obj message . args)
  (apply (obj message) args))

(define (mt)
  (let [(self 'dummy)]
    (begin
      (set! self (λ (m)
                   (case m
                     [(add) (λ () 0)])))
      self)))

(define (node value left right)
  (let [(self 'dummy)]
    (begin
      (set! self (λ (m)
                   (case m
                     [(add) (λ () (+ value
                                     (msg left 'add)
                                     (msg right 'add)))])))
      self)))

(define (mt/size parent-maker)
  (let ([parent (parent-maker)]
        [self 'dummy])
    (begin
      (set! self (λ (m)
                   (case m
                     [(size) (lambda () 0)]
                     [else (parent m)])))
      self)))

(define (node/size parent-maker value left right)
  (let ([parent (parent-maker value left right)]
        [self 'dummy])
    (begin
      (set! self (λ (m)
                   (case m
                     [(size) (λ () (+ 1
                                      (msg left 'size)
                                      (msg right 'size)))]
                     [else (parent m)])))
      self)))

; tests
(define a-tree/size
  (node/size node 10
             (node/size node 5 (mt/size mt) (mt/size mt))
             (node/size node 
                        15 
                        (node/size node 6 (mt/size mt) (mt/size mt))
                        (mt/size mt))))
(test (msg a-tree/size 'add) (+ 10 5 15 6))
(test (msg a-tree/size 'size) 4)
