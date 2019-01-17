#lang racket

(require test-engine/racket-tests)

; a structure to hold a digit, this is used by the lexer
; a token type will be 'op, 'lparen, 'rparen, or 'digit or 'eof
; repr is a character representation

; value node for numbers
(struct ast-node (val) #:transparent)
; expression nodes for operators 
(struct ast-expr-node (operator left-child right-child) #:transparent)

(struct token (type repr) #:transparent
   ; a guard is a function that 'tests' the values you put into the structure
   ; remember: racket is dynamically typed so you kinda have to check things to
   ; save yourself a ton of grief later (trust me)
   #:guard (λ (type repr struct-name)
     (if (not (is-token-type? type))
         (error "expected a proper token-type which is-token-type? returns true from, got" type)
         (if (and (not (eq? eof repr)) (not (char? repr)))
             (error "expected a string? or eof? for token-repr, got" repr)
             (values type repr)))))


; symbol -> bool
; returns true if the token matches the symbols 'op, 'lparen, 'rparen, 'digit
(define (is-token-type? t)
  (cond [(equal? 'op t) #t]
        [(equal? 'lparen t) #t]
        [(equal? 'rparen t) #t]
        [(equal? 'digit t) #t]
        [(equal? 'eof t) #t]
        [else #f]))

; input-port -> token
; returns the next input token from the input port
(define (get-next-token input-port)
  (let ([input (read-char input-port)])
    (cond
      [(equal? input #\+)     (token 'op input)]
      [(equal? input #\*)     (token 'op input)]
      [(equal? input #\()     (token 'lparen input)]
      [(equal? input #\))     (token 'rparen input)]
      [(equal? input #\0)     (token 'digit input)]
      [(equal? input #\1)     (token 'digit input)]
      [(equal? input #\2)     (token 'digit input)]
      [(equal? input #\3)     (token 'digit input)]
      [(equal? input #\4)     (token 'digit input)]
      [(equal? input #\5)     (token 'digit input)]
      [(equal? input #\6)     (token 'digit input)]
      [(equal? input #\7)     (token 'digit input)]
      [(equal? input #\8)     (token 'digit input)]
      [(equal? input #\9)     (token 'digit input)]
      [(equal? input eof)     (token 'eof input)]
      [else (error "Invalid input: " input)]
      )))
      

; string -> 0 argument function that returns the next token on the string
; this function creates a function that uses get-next-token on the string that was passed in,
; notice how we pass create the input by using open-input-string
(define (lexstr str)
 (let ([input (open-input-string str)])
 (λ () (get-next-token input))))

; (() -> token) -> (ast-node | ast-expr-node)
; the parser takes a function (probably produced by lexstr) that
; lexes the contents of the input stream
(define (parser lex)
  (let* ([tok (lex)]
         [rep (token-repr tok)]
         [typ (token-type tok)])
    (cond
      [(eq? typ 'eof)     '()]
      [(eq? typ 'digit)   (ast-node (string->number (string rep)))]
      [(eq? typ 'lparen)  (parse-expr lex)])))

(define (parse-expr lex)
  (let* ([exp1 (parser lex)]
         [op (parse-op lex)]
         [exp2 (parser lex)]
         [rparen? (lex)]
         [rparen-type (token-type rparen?)])
    (if (eq? rparen-type 'rparen)
        (ast-expr-node op exp1 exp2)
        (error "Invalid expression: " rparen-type))))

(define (parse-op lex)
  (let* ([tok (lex)]
         [typ (token-type tok)]
         [rep (token-repr tok)])
    (if (eq? typ 'op)
        rep
        (error "Invalid operation: " rep))))

(define (evaluate ast)
  (cond
    [(ast-node? ast)           (ast-node-val ast)]
    [(ast-expr-node? ast)
     (if (eq? #\+ (ast-expr-node-operator ast))
         (+ (evaluate (ast-expr-node-left-child ast)) (evaluate (ast-expr-node-right-child ast)))
         (* (evaluate (ast-expr-node-left-child ast)) (evaluate (ast-expr-node-right-child ast))))]))

(define (evalstr str)
  (evaluate (parser (lexstr str))))



(check-expect (is-token-type? 'op) #t)
(check-expect (is-token-type? 'lparen) #t)
(check-expect (is-token-type? 'rparen) #t)
(check-expect (is-token-type? 'digit) #t)
(check-expect (is-token-type? 'foo) #f)
(check-expect (is-token-type? 'eof) #t)

; error cases on creating tokens
(check-error (token 'op 1))
(check-error (token 'foo "hello"))

; test cases for get-next-token, note this implies get-next-token requires an input-port
; as its sole argument...(that's a hint)
(check-expect (get-next-token (open-input-string "0")) (token 'digit #\0))
(check-expect (get-next-token (open-input-string "1")) (token 'digit #\1))
(check-expect (get-next-token (open-input-string "2")) (token 'digit #\2))
(check-expect (get-next-token (open-input-string "3")) (token 'digit #\3))
(check-expect (get-next-token (open-input-string "4")) (token 'digit #\4))
(check-expect (get-next-token (open-input-string "5")) (token 'digit #\5))
(check-expect (get-next-token (open-input-string "6")) (token 'digit #\6))
(check-expect (get-next-token (open-input-string "7")) (token 'digit #\7))
(check-expect (get-next-token (open-input-string "8")) (token 'digit #\8))
(check-expect (get-next-token (open-input-string "9")) (token 'digit #\9))
(check-expect (get-next-token (open-input-string "+")) (token 'op #\+))
(check-expect (get-next-token (open-input-string "*")) (token 'op #\*))
(check-expect (get-next-token (open-input-string ")")) (token 'rparen #\)))
(check-expect (get-next-token (open-input-string "(")) (token 'lparen #\())
(check-expect (get-next-token (open-input-string "")) (token 'eof eof))
(check-error (get-next-token (open-input-string "a")))

; this should produce an error, it's not correct according to the grammar
(check-error (parser (lexstr "(5)")))
; but the rest of these are fine
(check-expect (parser (lexstr "5")) (ast-node 5))
(check-expect (parser (lexstr "(5+2)")) (ast-expr-node #\+ (ast-node 5) (ast-node 2)))
(check-expect (parser (lexstr "(5+(3+2))")) (ast-expr-node #\+ (ast-node 5) (ast-expr-node #\+ (ast-node 3) (ast-node 2))))
(check-expect (parser (lexstr "((5+3)+2)")) (ast-expr-node #\+ (ast-expr-node #\+ (ast-node 5) (ast-node 3)) (ast-node 2)))
(check-expect (parser (lexstr "((5+3)+(2*3))")) (ast-expr-node #\+ (ast-expr-node #\+ (ast-node 5) (ast-node 3)) (ast-expr-node #\* (ast-node 2) (ast-node 3))))

(check-expect (evalstr "5") 5)
(check-expect (evalstr "(5+2)") 7)
(check-expect (evalstr "(5+(3+2))") 10) 
(check-expect (evalstr "((5+3)+2)") 10) 
(check-expect (evalstr "((5+3)+(2*3))") 14)

(test)





                                        