#lang racket

(require "llvm-emitter-skeleton.rkt"
         "names.rkt"
         (prefix-in t: "types.rkt")
         "niparser.rkt"
         "typecheck.rkt"
         "errors.rkt"
         "log.rkt")

(provide (all-defined-out))

(define (trans str)
  (clear-errors)
  (clear-writers)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse error")
        (let ([ty (typecheck-ast ast)])
         (if (error-generated?)
             (error "cannot translate due to type error")
             (begin
               (ast->llvm (first ast))
               (finish-emission)))))))

  
(define (translate-str str)
  ; clear the errors first
  (clear-errors)
  (clear-writers)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse errors")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (begin
                (translate-ast ast)))))))

(define (translate-ast ast)
  ; begin with the prelude
  (emit-header)
  (emit-main-header)
  (ast->llvm (first ast))
  (emit-main-trailer)
  (finish-emission))

(define (ast->llvm ast)
  (let ([result (match ast
    ; deal with lists, like in let expressions
                  ['() '()]
                  [(cons first rest) (begin (ast->llvm first) (ast->llvm rest))]
    
                  ; integer literals
                  [(NumExpr val) (numexpr->llvm ast val)]

                  ;;math expressions
                  [(MathExpr _ _ _)  (math->llvm ast)]

                  ;;logic expressions
                  [(LogicExpr _ _ _) (logic->llvm ast)]

                  ;;boolean expressions
                  [(BoolExpr _ _ _) (boolcomp->llvm ast)]
                  ;;boolean literals
                  [(BoolVal val) (boolexpr->llvm ast val)]
    
                  [(StringExpr val) (stringexpr->llvm ast val)]

                  [(IfExpr _ _ '()) (if-then->llvm ast)]
                  [(IfExpr _ _ _) (if-then-else->llvm ast)]

                  ; variable declarations!
                  ;[(VarDecl _ _ _) (vardecl->llvm ast)]
    
                  ; function calls
                  [(FuncallExpr _ _) (funcall->llvm ast)]
       
                  ; variable expressions
                  ;[(VarExpr _) (var->llvm ast)]

                  ; let expressions--need these for any declarations to work!
                  ;[(LetExpr _ _) (letexpr->llvm ast)]
                  [_ (error "Translation node " ast " not implemented yet!")])])
    (add-note ast 'result result)
    result))


;;emits a call to a function
(define (funcall->llvm node)
  (emit-comment (string-append "calling function: "
                               (symbol->string
                                (FuncallExpr-name node))))
  (let* ([ty (get-note node 'type)]
         [funval (get-note node 'funval)]
         [funname ;(if (eq? (t:FunValue-label funval) #f)
                      (symbol->string (FuncallExpr-name node))]
                      ;(label-name (t:FunValue-label funval)))]
         [argtypes (map (λ (arg) (get-note arg 'type))
                        (FuncallExpr-args node))]
         [argresults (map (λ (arg) (ast->llvm arg) (get-note arg 'result))
                          (FuncallExpr-args node))])
    (let ([result (emit-funcall funname (get-note node 'type) argresults argtypes)])
      result)))
  

; emits a numeric literal
(define (numexpr->llvm node val)
  ; literal nums can go in registers
  (let ([result (emit-math 'add val "0")])
    (add-note node 'result result) result))

;;emits a string literal 
(define (stringexpr->llvm node val)
  (let ([result (emit-string val)])
    (add-note node 'result result) result))

;;emits a boolean literal
(define (boolexpr->llvm node val)
    (let* ([boolval (if val "1" "0")]
          [result (emit-bool boolval)])
      (add-note node 'result result) result))

;;emits a math expression
(define (math->llvm node)
  (let ([expr1 (MathExpr-expr1 node)]
        [expr2 (MathExpr-expr2 node)]
        [op (MathExpr-op node)])
    (let ([result1 (ast->llvm expr1)]
          [result2 (ast->llvm expr2)])
      (emit-math op result1 result2))))

;;emits a logic expression
(define (logic->llvm node)
  (let ([expr1 (LogicExpr-expr1 node)]
        [expr2 (LogicExpr-expr2 node)]
        [op (LogicExpr-op node)])
    (let ([result1 (ast->llvm expr1)]
          [result2 (ast->llvm expr2)])
      (emit-logic op result1 result2))))

;;emits a boolean expression
(define (boolcomp->llvm node)
  (let ([expr1 (BoolExpr-expr1 node)]
        [expr2 (BoolExpr-expr2 node)]
        [op (BoolExpr-op node)])
    (let ([result1 (ast->llvm expr1)]
          [result2 (ast->llvm expr2)])
      (if (t:StringType? (get-note expr1 'type))
          (emit-stringcomp op result1 result2)
      (emit-bool-comp op result1 result2)))))

;;emit an if then without else statement
(define (if-then->llvm node )
    (let ([test (IfExpr-test node)]
          [truebranch (IfExpr-true-branch node)])
      (emit-comment "if then")
      (let ([testres (ast->llvm test)]
            [truelabel (make-label)]
            [endlabel (make-label)])
        (emit-conditional-branch testres truelabel endlabel)
        (emit-comment "true branch")
        (emit-label truelabel)
        (ast->llvm truebranch)
        (emit-jump endlabel)
        (emit-label endlabel))))

(define (if-then-else->llvm node )
    (let ([test (IfExpr-test node)]
          [truebranch (IfExpr-true-branch node)]
          [falsebranch (IfExpr-false-branch node)])
      (emit-comment "if then else")
      (let ([testres (ast->llvm test)]
            [truelabel (make-label)]
            [falselabel (make-label)]
            [endlabel (make-label)])
        (emit-conditional-branch testres truelabel falselabel)
        (emit-comment "true branch")
        (emit-label truelabel)
        (let ([trueres (ast->llvm truebranch)])
          (emit-jump endlabel)
        
          (emit-comment "false branch")
          (emit-label falselabel)
          (let ([falseres (ast->llvm falsebranch)])
            (emit-jump endlabel)
        
        ;(emit-label endlabel)
            (if (not (eq? trueres #f))
                (begin (emit-label endlabel) (emit-phi (get-note node 'type) trueres truelabel falseres falselabel))
                (emit-label endlabel)))))))