#lang racket

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "Ni-lexer.rkt")

(provide (all-defined-out))

(struct VarDecl (type id expr) #:transparent)

(struct NameType(name kind next) #:transparent)
(struct RecordType (name fields next) #:transparent)
(struct ArrayType (name kind next) #:transparent)
(struct TypeField (kind name) #:transparent)
(struct FunDecl (name args rettype body next) #:transparent)
(struct NumExpr (val) #:transparent)
(struct VarExpr (name) #:transparent)
(struct BoolVal (val) #:transparent)
;;
(struct RecordExpr (name field) #:transparent)
(struct ArrayExpr (name expr) #:transparent)
(struct FuncallExpr (name args) #:transparent)
;;
;Done
(struct StringExpr (str) #:transparent)
(struct NoVal () #:transparent)
;
;Not done
(struct LetExpr (decs exprs) #:transparent)
(struct MathExpr (expr1 op expr2) #:transparent)
;
(struct BoolExpr (expr1 op expr2) #:transparent)
(struct LogicExpr (expr1 op expr2) #:transparent)
(struct FieldAssign (name expr) #:transparent)
;;Done
(struct NewRecordExpr (name assignments) #:transparent)
;;Not done
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

   ;;Think we need a precedence (left right etc)
   ;;Also need a way to fix order of operations with parentheses
   ;;List thing as well. 
   (grammar
    (expression
     ;;No Val
     [(LPAREN RPAREN)                                                                     (NoVal)]
     ;;nests the expression inside parens, inside of the other math expression
     ;;I think this is correct OoP
     [(LPAREN expression RPAREN)                                                          $2]
     [(BREAK)                                                                             (BreakExpr)]

     ;;should be legal to use an l-value as an expression
     
     ;;This existing as an expression causes a reduce/reduce which causes an error when doing a new array expression
     ;;the parser throws back the of token, and fails. 
     [(l-value)                                                                           $1]
     ;pretty sure declarations aren't expressions but dont' know
     ;;what they would be if not expressions
     ;;Variable declarations
     [(var-dec)                                                                           $1]

     [(LET var-dec IN expression END)                                                     (LetExpr $2 $4)]
     ;;this doesn't work, new array expression causes problems. 
     [(type-id LBRACKET expression RBRACKET OF expression)                                (NewArrayExpr $1 $3 $6)]
     [(ID LBRACE field-assignment RBRACE)                                                 (NewRecordExpr $1 $3)]

     ;;Math expression
     [(math-expr)                                                                         $1]
     
     ;;If expressions
     [(IF expression THEN expression END)                                                 (IfExpr $2 $4 '())]
     [(IF expression THEN expression ELSE expression END)                                 (IfExpr $2 $4 $6)]
     
     ;;Literals
     [(NUM)                                                                               (NumExpr $1)]
     [(STRING)                                                                            (StringExpr $1)]
     ;;Boolean literal struct needed?
     [(BOOL)                                                                              (BoolVal $1)]

     ;;Variable Assignment
     [(NOW l-value IS expression)                                                         (AssignmentExpr $2 $4)]
     [(NOW l-value IS PENG)                                                               (AssignmentExpr $2 (PengExpr))]

     ;;logic expressions
     [(expression BOOLAND expression)                                                     (LogicExpr $1 #\& $3)]
     [(expression BOOLOR expression)                                                      (LogicExpr $1 #\| $3)]

     ;;boolean expressions
     [(bool-expr)                                                                         $1]

     ;;Type definition
     [(DEFINE ty)                                                                         $2]

     ;;Function declarations and function calls
     [(func-dec)                                                                          $1]
     [(ID LPAREN args RPAREN)                                                             (FuncallExpr $1 $3)]
     ;;sequence
     [(LPAREN seq RPAREN)                                                                 $2]

     ;;loops
     [(WHILE expression DO expression END)                                                (WhileExpr $2 $4)]
     [(WITH ID AS expression TO expression DO expression END)                             (WithExpr $2 $4 $6 $8)])
    (var-dec
     [(NI type-id ID IS expression)                                                       (VarDecl $2 $3 $5)]
     [(NI type-id ID IS PENG)                                                             (VarDecl $2 $3 'peng)]
     [(NI ID IS expression)                                                               (VarDecl #f $2 $4)])
    (math-expr
     [(expression ADD expression)                                                         (MathExpr $1 '+ $3)]
     [(expression MULT expression)                                                        (MathExpr $1 '* $3)]
     [(expression DIV expression)                                                         (MathExpr $1 '\ $3)]
     [(expression SUB expression)                                                         (MathExpr $1 '- $3)]
     [(expression DOT expression)                                                         (MathExpr $1 #\. $3)]
     [(SUB expression)                                                                    (MathExpr 0 '- $2)])

    (bool-expr
     [(expression NE expression)                                                          (BoolExpr $1 'ne $3)]
     [(expression EQ expression)                                                          (BoolExpr $1 'eq $3)]
     [(expression LT expression)                                                          (BoolExpr $1 'lt $3)]
     [(expression LE expression)                                                          (BoolExpr $1 'le $3)]
     [(expression GT expression)                                                          (BoolExpr $1 'gt $3)]
     [(expression GE expression)                                                          (BoolExpr $1 'ge $3)])
    
    ;;The format that function call arguments can take, returns a list of the values
    (args
     [(expression COMMA args)                                                             (cons $1 $3)]
     [(expression)                                                                        (cons $1 '())])
    ;;l values
    (l-value
     [(ID)                                                                                (VarExpr $1)]
     [(l-value DOT ID)                                                                    (RecordExpr $1 $3)]
     [(l-value LBRACKET expression RBRACKET)                                              (ArrayExpr $1 $3)])
    ;;sequences
    ;;returns a list of expressions (in their respective structs)
    (seq
     [(expression SEMI seq)                                                               (cons $1 $3)]
     [(expression)                                                                        (cons $1 '())])
    ;;types, include recursive/non-recursive type decs for Name, Record, Array
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
     ;;hopefully this will only be ids that already have a type associated with them
     [(ID)                                                                                $1])
    (field-assignment
     [(ID IS expression)                                                                   (cons (FieldAssign $1 $3) '())]
     [(ID IS expression COMMA field-assignment)                                            (cons (FieldAssign $1 $3) $5)]))))


