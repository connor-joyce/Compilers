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
    (extend-env tenv 'x (types:make-IntType)) ;;Added this to test l-values
    (extend-env tenv 'void (types:make-VoidType)) ;;Added this back in because NoVal's should return Void (I think)
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
           [(VarDecl type name expr)    (let ([t1 (if (equal? type #f) #f (apply-env env (string->symbol type)))]
                                              [t2 (typecheck expr env)])
                                               (cond [(equal? t1 t2)   (types:make-VarValue t1)]
                                                     [(and (not (equal? t2 types:PengType)) (equal? type #f))  (types:make-VarValue t2)]
                                                     [else (error "Declared type: "t1" and given value type: "t2" must be the same\n")]))]
           [(VarExpr name)                (apply-env env (string->symbol name))]
           [(list)                      (printf "working")]
           [(TypeField _ typ)             (typecheck typ env)]
           [(IfExpr test true false)  (let* ([test-t (typecheck test env)]
                                            [true-t (typecheck true env)]
                                            [false-t (if (eq? false '()) true-t (typecheck false env))])
                                        (cond
                                          [(not (types:BoolType? test-t)) (error "Predicate must be bool type, actual type " test-t)]
                                          [(not (equal? false-t true-t)) (error "Both branches must be same type")]
                                          [else true-t]))]
           [(WhileExpr test body)    (let ([test-t (typecheck test env)]
                                           [body-t (typecheck body env)])
                                       (cond
                                         [(not (types:BoolType? test-t))   (error "Predicate must be bool type, actual type " test-t)]
                                         [else (typecheck body env)]))]
           [(ArrayExpr name index)   (let ([t1 (typecheck index env)]
                                           [t2 (apply-env env (string->symbol name))])
                                       (cond
                                         [(types:IntType? t1)        t2]
                                         [else                      (error "Index value must be of NumType")]))]
           [(NoVal)                  (types:make-VoidType)]
           [(BreakExpr)              (types:make-VoidType)]
           [(BoolExpr e1 op e2)      (let ([t1 (typecheck e1 env)]
                                           [t2 (typecheck e2 env)])
                                       (cond [(equal? t1 t2) (types:make-BoolType)]
                                             [else (error "Both expressions in a boolean comparison must be the same type")]))]
           [(MathExpr e1 op e2)
                             (let ([t1 (typecheck e1 env)]
                                   [t2 (typecheck e2 env)])
                               (cond [(and (types:IntType? t1) (types:IntType? t2))      (types:make-IntType)]
                                     [else (error "Both expressions need to be Number expressions in a math expression")]))]
           [(LogicExpr e1 op e2) 
                                       (let ([t1 (typecheck e1 env)]
                                             [t2 (typecheck e2 env)])
                                         (cond [(and (types:BoolType? t1) (types:BoolType? t2)) (types:make-BoolType)]
                                               [else (error "Both expressions in a logic expression must be boolean expressions")]))]
           
           #;[(NewArrayExpr name num-elements init-val)     (let*
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
           [(NameType name kind next)        (let*   ([next-type (λ () (if (not (eq? next '())) (typecheck next env) #f))]
                                                     [kind-type (apply-env env (string->symbol kind))])
                                               (cond
                                                 [(not (eq? kind-type #f))     (extend-env env (string->symbol kind) kind-type)
                                                                               (if (eq? next '()) kind-type (next-type))]
                                                 [else (error "21 pilots")]))]
           [(AssignmentExpr name expr)       (let* ([name-t (typecheck name env)]
                                                    [expr-t (typecheck expr env)])
                                               (cond
                                                 [(equal? name-t #f) (error "l-value not declared" name)]
                                                 [(not (equal? name-t expr-t)) (error "l-value and expression must be same type" name-t expr-t)]
                                                 [else expr-t]))]
                                                   
           [_                      (error "Type check error")])])
    type-of-expr))
