#lang racket

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "Ni-lexer.rkt")

(require test-engine/racket-tests)

(provide (all-defined-out))

(struct VarDecl (type id expr) #:transparent)

(struct NameType(name kind next) #:transparent)
(struct RecordType (name fields next) #:transparent)
(struct ArrayType (name kind next) #:transparent)
(struct TypeField (name kind) #:transparent)
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
  (start program)
  (end EOF)
  (tokens value-tokens paren-types operators punctuation comparators boolops keywords endoffile)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
         (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
             (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                     (lex:position-line start-pos) (lex:position-col start-pos) tok-name tok-value tok-ok?))))

   ;;Think we need a precedence (left right etc)
   (grammar
    (program
     [(expression)       (list $1)])
    (expression
     [(l-value)                    $1]
    ; [(valueless-expr)            $1]
     [(PENG)                      (PengExpr)]
     [(LPAREN seq RPAREN)                       $2]
     [(LPAREN RPAREN)             (NoVal)]
     [(NUM)                       (NumExpr $1)]
     [(STRING)                    (StringExpr $1)]
     [(BOOL)                      (BoolVal $1)]
     [(expression mathop expression)  (MathExpr $1 $2 $3)]
     [(expression boolop expression)  (BoolExpr $1 $2 $3)]
     ;;boolops should also include string ops
     [(expression logicop expression) (LogicExpr $1 $2 $3)]
     [(type-dec)                            $1]
     [(var-dec)                             $1]
     [(func-dec)                            $1]
     [(ID LPAREN args RPAREN)               (FuncallExpr $1 $3)]
     [(NOW l-value IS expression)           (AssignmentExpr $2 $4)]
     [(if)                                  $1]
     [(loops)                               $1]
     ;This line causes an error with array expressions
     [(ID LBRACKET expression RBRACKET OF expression)     (NewArrayExpr $1 $3 $6)]
     [(type-id LBRACE field-assn RBRACE)                       (NewRecordExpr $1 $3)]
     [(BREAK)                               (BreakExpr)]
     [(LET decs IN seq END)                 (LetExpr $2 $4)]
     [(LPAREN expression RPAREN)            $2]
     [(SUB expression)                      (MathExpr (NumExpr "0") '- $2)])
     ;[()                                    '()])

    (args
     [()                                    '()]
     [(expression)                          (cons $1 '())]
     [(expression COMMA args)         (cons $1 $3)])

    (l-value
     [(l-value DOT ID)                      (RecordExpr $1 $3)]
     ;;Changing l-value to ID fixes the error but isn't correct
     ;[(l-value LBRACKET expression RBRACKET)(ArrayExpr $1 $3)]
     [(ID LBRACKET expression RBRACKET)     (ArrayExpr $1 $3)]
     [(ID)                                  (VarExpr $1)])

    (seq
     [()                                     '()]    
     [(expression SEMI seq)                  (cons $1 $3)]
     [(expression)                           (cons $1 '())])

    (mathop
     [(ADD)                                   '+]
     [(SUB)                                   '-]
     [(MULT)                                  '*]
     [(DIV)                                   '/])

    (boolop
     [(EQ)                                    'eq]
     [(NE)                                    'ne]
     [(GT)                                    'gt]
     [(LT)                                    'lt]
     [(LE)                                    'le]
     [(GE)                                    'ge])

    (logicop
     [(BOOLAND)                                   '&]
     ;;cant make a symbol out of the pipe for some reason
     [(BOOLOR)                                    #\|])

    (type-dec
     [(DEFINE ID KIND AS LBRACE typefields RBRACE) (RecordType $2 $6 '())]
     [(DEFINE ID KIND AS ARRAY OF type-id)         (ArrayType $2 $7 '())]
     [(DEFINE ID KIND AS LBRACE typefields RBRACE AND type-dec) (RecordType $2 $6 $9)]
     [(DEFINE ID KIND AS ARRAY OF type-id AND type-dec) (ArrayType $2 $7 $9)]
     [(DEFINE ID KIND AS type-id)                       (NameType $2 $5 '())]
     [(DEFINE ID KIND AS type-id AND type-dec)          (NameType $2 $5 $7)])

    (typefields
     [(type-id ID)                                 (cons (TypeField $2 $1) '())]
     [(type-id ID COMMA typefields)                (cons (TypeField $2 $1) $4)]
     [()                                                      '()])

    (var-dec
     [(NI ID IS expression)                   (VarDecl #f $2 $4)]
     [(NI type-id ID IS expression)           (VarDecl $2 $3 $5)])

    (func-dec
     [(NEEWOM ID LPAREN typefields RPAREN IS expression)  (FunDecl $2 $4 #f $7 '())]
     [(NEEWOM ID LPAREN typefields RPAREN AS type-id IS expression) (FunDecl $2 $4 $7 $9 '())]
     [(NEEWOM ID LPAREN typefields RPAREN IS expression AND func-dec) (FunDecl $2 $4 #f $7 $9)]
     [(NEEWOM ID LPAREN typefields RPAREN AS type-id IS expression AND func-dec) (FunDecl $2 $4 $7 $9 $11)])

    (if
     [(IF expression THEN expression END)     (IfExpr $2 $4 '())]
     [(IF expression THEN expression ELSE expression END) (IfExpr $2 $4 $6)])

    (loops
     [(WHILE expression DO expression END)   (WhileExpr $2 $4)]
     [(WITH ID AS expression TO expression DO expression END) (WithExpr $2 $4 $6 $8)])

    (type-id
     [(ID)                                   $1])

    (decs
     [(var-dec)                             (cons $1 '())]
     [(func-dec)                            (cons $1 '())]
     [(type-dec)                            (cons $1 '())]
     [(type-dec decs)                       (cons $1 $2)]
     [(var-dec decs)                        (cons $1 $2)]
     [(func-dec decs)                       (cons $1 $2)])
    (field-assn
     [(ID IS expression)                (cons (FieldAssign $1 $3) '())]
     [(ID IS expression COMMA field-assn) (cons (FieldAssign $1 $3) $5)]
     [()                                 '()]))))
    
     



; var declarations
(check-expect (parse-str "ni x is 5") (list (VarDecl #f "x" (NumExpr "5"))))
; type declarations
(check-expect (parse-str "define int2 kind as int") (list (NameType "int2" "int" '())))
(check-expect (parse-str "define intarr kind as array of int") (list (ArrayType "intarr" "int" '())))
(check-expect (parse-str "define intrec kind as { int x }")
              (list (RecordType "intrec" (list (TypeField "x" "int")) '())))
; function declarations
(check-expect (parse-str "neewom getX() as int is 5")
              (list (FunDecl "getX" '() "int" (NumExpr "5") '())))
; function calls of various sorts
(check-expect (parse-str "add2(5)") (list (FuncallExpr "add2" (list (NumExpr "5")))))
; parens
(check-expect (parse-str "(5)") (list (NumExpr "5")))
; various sequences
(check-expect (parse-str "(6; 5)") (list (list (NumExpr "6") (NumExpr "5"))))
; strings
(check-expect (parse-str "\"Hello World\"") (list (StringExpr "\"Hello World\"")))
; noval
(check-expect (parse-str "()") (list (NoVal)))
; let expressions
(check-expect (parse-str "let ni x is 5 in x end")
              (list (LetExpr (list (VarDecl #f "x" (NumExpr "5"))) (list (VarExpr "x")))))
; math ops
(check-expect (parse-str "1+2")
              (list (MathExpr (NumExpr "1") '+ (NumExpr "2"))))
; math ops using negated numbers
(check-expect (parse-str "-5") (list (MathExpr (NumExpr "0") '- (NumExpr "5"))))

; bool expressions
(check-expect (parse-str "5=6") (list (BoolExpr (NumExpr "5") 'eq (NumExpr "6"))))

; array creation
(check-expect (parse-str "intarr[10] of 6")
              (list (NewArrayExpr "intarr" (NumExpr "10") (NumExpr "6"))))

; record expression
(check-expect (parse-str "point { x is 6 }")
              (list (NewRecordExpr "point" (list (FieldAssign "x" (NumExpr "6"))))))

(test)


