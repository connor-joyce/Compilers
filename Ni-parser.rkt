#lang racket

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "Ni-lexer.rkt")

(provide (all-defined-out))

(struct VarDecl (type id expr) #:transparent)

(struct NameType(name kind next) #:transparent)
(struct RecordType (name fields next) #:transparent)
(struct ArrayType (name kind next) #:transparent)
(struct TypeField (name kind) #:transparent)

(struct FunDecl (name args rettype body next) #:transparent)

(struct NumExpr (val) #:transparent)
(struct VarExpr (name) #:transparent)
(struct Bool (val) #:transparent)
(struct RecordExpr (name field) #:transparent)
(struct ArrayExpr (name expr) #:transparent)
(struct FuncallExpr (name args) #:transparent)
(struct StringExpr (str) #:transparent)
(struct NoVal () #:transparent)
(struct LetExpr (decs exprs) #:transparent)
(struct MathExpr (expr1 op expr2) #:transparent)
(struct BoolExpr (expr1 op expr2) #:transparent)
(struct LogicExpr (expr1 op expr2) #:transparent)
(struct FieldAssign (name expr) #:transparent)
(struct NewRecordExpr (name assignments) #:transparent)
(struct NewArrayExpr (name expr kind) #:transparent)
(struct IfExpr (test true-branch false-branch) #:transparent)
(struct WhileExpr (test body) #:transparent)
(struct AssignmentExpr (name expr) #:transparent)
(struct BreakExpr () #:transparent)
(struct PengExpr () #:transparent)
(struct WithExpr (idname initexpr fromexpr toexpr) #:transparent)

; input port -> ni ast   
; construct an ast from the input port
(define (build-ast in)
  (port-count-lines! in)
  (ni-parser (get-tokenizer in)))

; string representing ni code -> ni ast
; parses a string and turns it into an ast if possible
(define (parse-str str)
  (let ([in (open-input-string str)])
    (build-ast in)))

; string (filename) -> ni ast
; opens and parses a file and tries to turn it into an ast if possible
(define (parse-file filename)
  (let ([in (open-input-file filename)])
    (build-ast in)))

;;error definition to go in the parser

(define ni-parser
  (parser
  (src-pos)
  (start expression)
  (end EOF)
  (tokens value-tokens paren-types operators punctuation comparators boolops keywords endoffile)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
         (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
             (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                     (lex:position-line start-pos) (lex:position-col start-pos) tok-name tok-value tok-ok?))))
   (grammar

    (expression
     ;pretty sure declarations aren't expressions but dont' know
     ;;how to 
     [(NI type-id ID IS expression)   (VarDecl $2 $3 $5)]
     [(NI ID IS expression)           (VarDecl #f $2 $4)]
     [(DEFINE ID KIND AS ty)          $2]
     ;;
     [(NUM)                           (NumExpr $1)]
     [(STRING)                        (StringExpr $1)]
     ;;Boolean literal struct needed?
     [(BOOL)                          (Bool $1)])
    (ty
     [(type-id)                       $1])
    (type-id
     [(ID)                            $1]))))



     
     
     
   
   

