#lang plai-typed

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])


(define ma1 (caml 2))
(define ma2 (yacc 1.9))

(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
    [caml (humps) (>= humps 2)]
    [yacc (height) (> height 2.1)])
  )

(test (good? ma1) #t)
(test (good? ma2) #f)

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([s1 (s-exp->list s)])
       (case (s-exp->symbol (first s1))
         [(+) (plusC (parse (second s1)) (parse (third s1)))]
         [(*) (multC (parse (second s1)) (parse (third s1)))]))]
    [else (error 'parse "invalid list input")]))





