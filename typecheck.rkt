#lang racket
(require (prefix-in types: "types.rkt")
         "Ni-parser.rkt"
         "environment.rkt")

(provide (all-defined-out))

;;initialize emtpy environment, then add base types
(define (init-typeEnv)
  (let ([tenv (empty-env)])
    (extend-env tenv 'int (types:make-IntType))
    (extend-env tenv 'bool (types:make-BoolType))
    (extend-env tenv 'string (types:make-StringType))
    ;(extend-env tenv 'void (types:make-VoidType)) ;;Jeff's not sure about this one, but I am
    (extend-env tenv 'peng (types:make-PengType))))

(define typeEnv (make-parameter (init-typeEnv)))

(define (init-typechecker)
  (typeEnv (init-typeEnv)))

(define (tc-str str)
  (init-typechecker)
  ;;ast should be a list of structs. We want to typecheck them one by one
  ;;my ast is just a struct rn
  (let ([ast (parse-str str)])
    (let ([ty (typecheck (first ast) (typeEnv))])
          ty)))

(define (typecheck ast env)
  (let ([type-of-expr
         (match ast
           [(NumExpr _)               (types:make-IntType)]
           [(StringExpr _)            (types:make-StringType)]
           [(BoolVal _)               (types:make-BoolType)]
           [(PengExpr)                (types:make-PengType)]
           [(MathExpr e1 op e2)
                             (let ([t1 (typecheck e1 env)]
                                   [t2 (typecheck e2 env)])
                               (cond [(and (types:IntType? t1) (types:IntType? t2))      (types:make-IntType)]))]
           [(NewArrayExpr name num-elements init-val)     (let*
                                                                ([arrty (types:actual-type (apply-env typeEnv name))]
                                                                 [countty (types:actual-type (typecheck num-elements typeEnv))]
                                                                 [initty (types:actual-type (typecheck init-val typeEnv))])
                                                              (cond
                                                                [(not (types:ArrayType? arrty))
                                                                 ;;errors should be log-typeerror
                                                                 (begin (error "~a must be an array type" ast name) (types:make-VoidType))]
                                                                 [(not (types:IntType? countty))
                                                                  (begin (error "number of elements in an array must be an int type" ast)
                                                                         (types:make-VoidType))]
                                                                 ;;types:type=? isn't defined yet, try to figure out this helper. 
                                                                 [(not (types:type=? initty (types:ArrayType-element-type arrty)))
                                                                  (begin (error "initialization value for array doesn't match type of array elements" ast)
                                                                         (types:make-VoidType))])
                                                                arrty)]
           [_                      (error "Type check error")])])
    type-of-expr))