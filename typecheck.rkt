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
    (extend-env tenv 'void (types:make-VoidType)) ;;Added this back in because NoVal's should return Void (I think)
    (extend-env tenv 'peng (types:make-PengType))
    ;(extend-env (push-scope tenv) 'test (types:make-RecordType 'test (list(types:NameTypePair (types:make-IntType) 'x))))
    ))
    

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
           ;;if ast is a list of anything then get the first and the rest
           ;;if the rest is empty-list then typecheck the first and exit
           ;;if the rest is not empty then typecheck the first and recursively type check the rest
           ;;return the last typechecked value
           ;['() (types:make-VoidType)]

           ;;doing this separates the two list matches
           ;;however it defaults to the sequence match on an empty list which causes problems

           ;;now the program is trying to pull out the symbol of the void type, but it doesn't get saved properly because its not a field
           ;;assign
           ['()  (cons (types:make-VoidType) '())]
           [(list (TypeField _ _) _ ...)  (let* ([fr (first ast)]
                                           [rs (rest ast)])
                                        (cond
                                          [(equal? '() rs)          (cons (typecheck fr env) '())]
                                          [else                     (cons (typecheck fr env) (typecheck rs env))]))]
           
           [(list _ ... _)    (let* ([fr (first ast)]
                                     [rs (rest ast)])
                                        (cond
                                          [(equal? fr '())          (types:make-VoidType)]
                                          [(equal? '() rs)          (typecheck fr env)]
                                          [else                     (typecheck fr env) (typecheck rs env)]))]
           [(NumExpr _)               (types:make-IntType)]
           [(StringExpr _)            (types:make-StringType)]
           [(BoolVal _)               (types:make-BoolType)]
           [(PengExpr)                (types:make-PengType)]
           ;;I don't think we save the var value, we just save the type of the var to teh environment
           [(VarDecl type name expr)    (let* ([t2 (typecheck expr env)]
                                              [t1 (if (equal? type #f) #f (apply-env env (string->symbol type)))]
                                              [var-t t2])
                                               (cond [(equal? t1 t2)   (extend-env env (string->symbol name) var-t) var-t]
                                                     [(and (equal? t2 types:PengType) (not (equal? t1 #f))) (extend-env env (string->symbol name) types:PengType) types:PengType]
                                                     [(and (not (equal? t2 types:PengType)) (not (equal? t1 #f))) (extend-env env (string->symbol name) t2) t2]
                                                     [(and (not (equal? t2 types:PengType)) (equal? t1 #f)) (extend-env env (string->symbol name) t2) t2]
                                                     [else (error "Declared type: "t1" and given value type: "t2" must be the same\n")]))]
           [(VarExpr name)                (apply-env env (string->symbol name))]

           [(TypeField name typ)       (let* ([sym (string->symbol name)]
                                              [name-t? (apply-env-scope env sym)] ;; don't care if the name exists in the environment only in the current scope
                                              [typ-t (apply-env env (string->symbol typ))])
                                         (cond
                                           [(not (equal? #f name-t?))  (error "identifier already in environment" name)]
                                           [(equal? #f typ-t)          (error "typ must exist in environment" typ)]
                                           [else (extend-env env sym typ-t)(types:NameTypePair typ-t sym)]))]
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
           ;;Not sure if the scope persists across multiple recursions/function calls
           [(WithExpr id val test body)  (let* ([val-t (typecheck val env)]
                                                [new-env (extend-env (push-scope env) (string->symbol id) val-t)]
                                               [test-t (typecheck test new-env)]
                                               [body-t (typecheck body new-env)])
                                           (cond
                                             [(not (equal? val-t test-t)) (error "Value and condition must be same type" val-t test-t)]
                                             [(not (types:IntType? test-t)) (error "Value and condition must be num types" val-t test-t)]
                                             [else (pop-scope new-env) body-t]))]
           ;;type of name is array type, but we need type of the given element
           ;;which is the element-type of the array type
           [(ArrayExpr name index)   (let*([t1 (typecheck index env)]
                                           [t2 (apply-env env (string->symbol name))]
                                           [e-type (types:ArrayType-element-type t2)])
                                       (cond
                                         [(equal? #f t2)             (error "unbound identifier" name)]
                                         [(not (types:IntType? t1))  (error "Index value must be an int type" t1)]
                                         [else        e-type]))]
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
                                     [else (error "Both expressions need to be Number expressions in a math expression" e1 op e2)]))]
           [(LogicExpr e1 op e2) 
                                       (let ([t1 (typecheck e1 env)]
                                             [t2 (typecheck e2 env)])
                                         (cond [(and (types:BoolType? t1) (types:BoolType? t2)) (types:make-BoolType)]
                                               [else (error "Both expressions in a logic expression must be boolean expressions")]))]
           
           [(ArrayType name kind '())                     (let* ([kind-t (apply-env env (string->symbol kind))]
                                                                 [l-exists? (apply-env env (string->symbol name))]
                                                                 [arr-t (types:make-ArrayType '() kind-t)])
                                                            (cond
                                                              [(equal? #f kind-t)   (error "kind must be an existing type" kind)]
                                                              [l-exists?     (error "Identifier value already exists" name)]
                                                              [else          (extend-env env (string->symbol name) arr-t) arr-t]))]

           [(ArrayType name kind next)                   (let* ([next-t (typecheck next env)]
                                                                [kind-t (apply-env env (string->symbol kind))]
                                                                 [l-exists? (apply-env env (string->symbol name))]
                                                                 [arr-t (types:make-ArrayType '() kind-t)])
                                                            (cond
                                                              [(equal? #f kind-t)   (error "kind must be an existing type" kind)]
                                                              [l-exists?     (error "Identifier value already exists" name)]
                                                              [else          (extend-env env (string->symbol name) arr-t) next-t]))]

           [(NameType name kind '())                   (let ([kind-t (apply-env env (string->symbol kind))]
                                                              [l-exists (apply-env env (string->symbol name))])
                                                         (cond
                                                           [(equal? kind-t #f)    (error "kind must be an existing type" kind)]
                                                           [l-exists  (error "name already in use elsewhere" name kind kind-t l-exists)]
                                                           [else                  (extend-env env (string->symbol name) kind-t) kind-t]))]
           
           [(NameType name kind next)                  (let ([next-t (typecheck next env)]
                                                              [kind-t (apply-env env (string->symbol kind))]
                                                              [l-exists (apply-env env (string->symbol name))])
                                                         (cond
                                                           [(equal? kind-t #f)    (error "kind must be an existing type" kind)]
                                                           [l-exists  (error "name already in use elsewhere" l-exists)]
                                                           [else                  (extend-env env (string->symbol name) kind-t) next-t]))]
           [(NewArrayExpr type-id length init)         (let* ([id-t (apply-env env (string->symbol type-id))]
                                                              [length-t (typecheck length env)]
                                                              [init-t (typecheck init env)])
                                                         (cond
                                                           [(equal? id-t #f)   (error "type does not exist in the environment" type-id)]
                                                           [(not (types:ArrayType?  id-t)) (error "type must be array type" id-t)]
                                                           [(not (types:IntType? length-t))  (error "length must be of type int" length-t)]
                                                           [(not (equal? init-t (types:ArrayType-element-type id-t))) (error "init value type must be same type as element type in array" init-t)]
                                                           [else id-t]))]

           [(types:VarValue type _ _ _ _ _)     type]

           ;;We can't test this and we have no idea if it works
           ;;We hope that the required errors are handled in the lower calls of typecheck and the environment persists
           ;;Declarations aren't working so we can't test this
           [(LetExpr decs exprs)            (let* ([new-env (push-scope env)]
                                                   [decs-t (typecheck decs new-env)]
                                                   [expr-check (typecheck exprs new-env)]
                                                   [exprs-t (if (list? expr-check) (first expr-check) expr-check)])
                                              (cond
                                                [(not (equal? exprs-t '())) exprs-t]
                                                [else (types:make-VoidType)]))]
           
           [(FieldAssign name expr)          (let ([name-t (apply-env env (string->symbol name))]
                                                   [expr-t (typecheck expr env)])
                                               (cond
                                                 [(equal? name-t #f)      (error "l-value must be present in env" name)]
                                                 [(types:PengType? name-t) (types:make-VoidType)]
                                                 [(types:PengType? expr-t) (types:make-VoidType)]
                                                 [(not(equal? name-t expr-t))  (error "l-value and r-value must have same types" name-t expr-t)]
                                                 ;;Don't think we need to extend environment, because the type remains the same
                                                 [else expr-t]))]
           [(AssignmentExpr name expr)       (let* ([name-t (typecheck name env)]
                                                    [expr-t (typecheck expr env)])
                                               (cond
                                                 [(equal? name-t #f) (error "l-value not declared" name)]
                                                 [(types:PengType? name-t) (types:make-VoidType)]
                                                 [(types:PengType? expr-t) (types:make-VoidType)]
                                                 [(not (equal? name-t expr-t)) (error "l-value and expression must be same type" name-t expr-t)]
                                                 [else (types:make-VoidType)]))]
           [(RecordType name fields '())    (let* ([rec-env (push-scope env)]
                                                   [name-t? (apply-env env (string->symbol name))]
                                                   [temp-t (extend-env env (string->symbol name) (types:make-RecordType (string->symbol name) '()))]
                                                   [field-t-list (typecheck fields rec-env)]
                                                   [rec-t (types:make-RecordType (string->symbol name) field-t-list)])
                                              (cond
                                                [(not(equal? name-t? #f))  (error "identifier already bound" name)]
                                                [else (pop-scope env)(extend-env env (string->symbol name) rec-t) rec-t]))]
           [(RecordType name fields next)    (let* ([rec-env (push-scope env)]
                                                    [next-t (typecheck next env)]
                                                   [name-t? (apply-env env (string->symbol name))]
                                                   [field-t-list (typecheck fields rec-env)]
                                                   [rec-t (types:make-RecordType (string->symbol name) field-t-list)])
                                              (cond
                                                [(not(equal? name-t? #f))  (error "identifier already bound" name)]
                                                [else (pop-scope env) (extend-env env (string->symbol name) rec-t) next-t]))]
           
           [(RecordExpr name field)          (let* ([rec-t (typecheck name env)]
                                                    ;[fields (if (types:VoidType? (first types:RecordType-fields rec-t))
                                                    [field-t (field-helper (string->symbol field) (types:RecordType-fields rec-t))])
                                               (cond
                                                 [(equal? rec-t #f)        (error "Record type does not exist" name)]
                                                 [(equal? field-t #f)      (error "Field does not exist in record type" field)]
                                                 [else                     field-t]))]

           [(NewRecordExpr name assns)       (let* ([rec-t (apply-env env (string->symbol name))]
                                                    [temp-env (type-field-env (push-scope env) (types:RecordType-fields rec-t))]
                                                    [assn-t (typecheck assns temp-env)])
                                                rec-t)]

           [(FunDecl name args rettype body '()) (let* (
                                                         [sym (string->symbol name)]
                                                         [exists? (apply-env env sym)]
                                                         [fun-env (push-scope env)]
                                                         [args-list (typecheck args fun-env)]
                                                         [body-type (typecheck body fun-env)]
                                                         [ret-type (if (equal? rettype #f) body-type (apply-env env (string->symbol rettype)))]
                                                         )
                                                    (cond
                                                      [(not (equal? exists? #f))   (error "Identifier already exists in the environment" sym)]
                                                      [(not (equal? ret-type body-type)) (error "Body type and return type must match" body-type ret-type)]
                                                      [else (pop-scope env)(extend-env env sym (types:make-FunValue sym args-list ret-type)) ret-type]))]

           [(FunDecl name args rettype body next) (let* ([next-type (typecheck next env)]
                                                         [sym (string->symbol name)]
                                                         [exists? (apply-env env sym)]
                                                         [fun-env (push-scope env)]
                                                         [args-list (typecheck args fun-env)]
                                                         [body-type (typecheck body fun-env)]
                                                         [ret-type (if (equal? rettype #f) body-type (apply-env env (string->symbol rettype)))]
                                                         )
                                                    (cond
                                                      [(not (equal? exists? #f))   (error "Identifier already exists in the environment" sym)]
                                                      [(not (equal? ret-type body-type)) (error "Body type and return type must match" body-type ret-type)]
                                                      [else (extend-env env 'sym (types:make-FunValue sym args-list ret-type)) next-type]))]
           [(FuncallExpr name args)               (let* ([sym (string->symbol name)]
                                                         [exists? (apply-env env sym)])
                                                    (cond
                                                      [(not exists?)    (error "function does not exist" sym)]
                                                      [(not(check-args (types:FunValue-parameters exists?) args env))   (error "args do not typematch parameters")]
                                                      [else (types:FunValue-return-type exists?)]))]
                                                   
           [_                      (error "Type check error" ast)])])
    type-of-expr))

(define (check-args src check env)
  (cond
    [(and (equal? src '()) (equal? check '()))  #t]
    [(and (types:VoidType? (car src)) (equal? check '())) #t]
    [(equal? (types:NameTypePair-type (car src)) (typecheck (first check) env)) (check-args (cdr src) (cdr check) env)]
    [else #f]))

(define (field-helper field field-list)
  (cond
    [(equal? field-list '())                       #f]
    [(equal? field-list types:VoidType)            #f]
    [(equal? field (types:NameTypePair-name (car field-list)))         (types:NameTypePair-type (car field-list))]
    [else                                          (field-helper field (rest field-list))]))

(define (type-field-env env field-list)
  (cond
    [(equal? field-list '())        env]
    [(types:VoidType? (first field-list)) env]
    [else (extend-env env (types:NameTypePair-name (car field-list)) (types:NameTypePair-type (car field-list))) (type-field-env env (cdr field-list))]))
