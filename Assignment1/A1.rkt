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



