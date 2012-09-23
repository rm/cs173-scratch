#lang plai-typed

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(define (good? [ma : MisspelledAnimal])
  (type-case MisspelledAnimal ma
    [caml (h) (>= h 2)]
    [yacc (h) (>= h 3)]))

(define ma1 (caml 2))
(define ma2 (yacc 1.9))

(print-only-errors #t)

(test (good? ma1) #t)
(test (good? ma2) #f)