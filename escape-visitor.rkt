#lang racket

(require "environment.rkt"
         "types.rkt"
         "Ni-parser.rkt")

(provide (all-defined-out))

(define escape-env (empty-env))

(struct escape-info (escape? level))

;;public facing function
(define (escape-visitor ast)
  (escape-visitor-helper ast 0)) ;<-- public facing function calls backend helper which starts at level one

;;assume this has passed through the typechecker
;;dont' check to see if variables are declared before usage
(define (escape-visitor-helper ast level)
  (match (first ast)
    ;;all possible structs 
    [(BoolExpr e1 op e2)  (escape-visitor-helper e1 level)
                          (escape-visitor-helper e2 level)]
    [(MathExpr e1 op e2)  (escape-visitor-helper e1 level)
                          (escape-visitor-helper e2 level)]
    [(VarDecl _ id expr)  (extend-env escape-env id (escape-info #f level))
                          (escape-visitor-helper expr level)]
    [(VarExpr name)       (let ([var (apply-env escape-env name)])
                                (var-escape-info-escape? #true))]))
                                
  