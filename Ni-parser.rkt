#lang racket

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "Ni-lexer.rkt")

(provide (all-defined-out))

(struct VarDecl (type id expr) #:transparent)

(struct NameType(name kind next) #:transparent)
(struct RecordType (name fields next) #:transparent)
;;flipped kind and name because I put them in backwards, like everywhere. 
(struct ArrayType (kind name next) #:transparent)
(struct TypeField (kind name) #:transparent)

(struct FunDecl (name args rettype body next) #:transparent)

(struct NumExpr (val) #:transparent)
(struct VarExpr (name) #:transparent)
(struct Bool (val) #:transparent)
;;
(struct RecordExpr (name field) #:transparent)
(struct ArrayExpr (name expr) #:transparent)
(struct FuncallExpr (name args) #:transparent)
;;
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
     ;;what they would be if not expressions 
     [(NI type-id ID IS expression)                                                       (VarDecl $2 $3 $5)]
     [(LPAREN RPAREN)                                                                     (NoVal)]
     [(NI ID IS expression)                                                               (VarDecl #f $2 $4)]
     [(IF expression THEN expression END)                                                 (IfExpr $2 $4 '())]
     [(IF expression THEN expression ELSE expression END)                                 (IfExpr $2 $4 $6)]
     [(NUM)                                                                               (NumExpr $1)]
     [(STRING)                                                                            (StringExpr $1)]
     [(ID)                                                                                (VarExpr $1)]
     [(expression BOOLAND expression)                                                     (BoolExpr $1 #\& $3)]
     [(expression BOOLOR expression)                                                      (BoolExpr $1 #\| $3)]
     ;;Boolean literal struct needed?
     [(BOOL)                                                                              (Bool $1)]
     [(DEFINE ty)                                                                         $2]
     [(func-dec)                                                                          $1]
     [(ID LBRACE assignment RBRACE)                                                       (NewRecordExpr $1 $3)]
        ;;pretty sure assignment causes shift/reduce conflict because it could be infinite recursion
     [(assignment)                                                                        $1]
     [(LPAREN seq RPAREN)                                                                 $2]
     [(WHILE expression DO expression END)                                                (WhileExpr $2 $4)])
    (seq
     [(expression SEMI seq)                                                               (cons $1 $3)]
     [(expression)                                                                        (cons $1 '())])
    (ty
     [(ID KIND AS type-id)                                                                (NameType $1 $4 '())]
     [(ID KIND AS LBRACE typefields RBRACE)                                               (RecordType $1 $5 '())]
     [(ID KIND AS ARRAY OF type-id)                                                       (ArrayType $1 $6 '())]
     ;;mutually recursive type decs
     [(ID KIND AS type-id AND DEFINE ty)                                                  (NameType $1 $4 $7)]
     [(ID KIND AS LBRACE typefields RBRACE AND DEFINE ty)                                 (RecordType $1 $5 $9)]
     [(ID KIND AS ARRAY OF type-id AND DEFINE ty)                                         (ArrayType $1 $6 $9)])
    (func-dec
     ;;mutually recursive function decs (causing shift/reduce errors but don't know how to fix)
     ;;could be because it could cause infinite recursion. Once everything works try making the func-dec and mut-func-dec
     ;;fields different, that limits the recursion to once. 
     [(NEEWOM ID LPAREN typefields RPAREN IS expression AND func-dec)                     (FunDecl $2 $4 '() $7 $9)]         
     [(NEEWOM ID LPAREN typefields RPAREN AS type-id IS expression AND func-dec)          (FunDecl $2 $4 $7 $9 $11)]
     ;;non-recursive function decs
     [(NEEWOM ID LPAREN typefields RPAREN IS expression)                                  (FunDecl $2 $4 '() $7 '())]         
     [(NEEWOM ID LPAREN typefields RPAREN AS type-id IS expression)                       (FunDecl $2 $4 $7 $9 '())])
    (typefields
     [(type-id ID)                                                                        (cons (TypeField $1 $2) '())]
     [(type-id ID COMMA typefields)                                                       (cons (TypeField $1 $2) $4)]
     [()                                                                                  '()])
    (type-id
     [(ID)                                                                                $1])
    (assignment
     [(ID IS expression)                                                                   (cons (FieldAssign $1 $3) '())]
     [(ID IS expression COMMA assignment)                                                  (cons (FieldAssign $1 $3) $5)]))))


   

