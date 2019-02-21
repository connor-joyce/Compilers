#lang racket
; a structure to hold a digit, this is used by the lexer
; a token type will be 'op, 'lparen, 'rparen, or 'digit or 'eof
; repr is a character representation
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
        [else #f]))

; input-port -> token
; returns the next input token from the input port
(define (get-next-token input-port)
  (let ([input (string-ref (read-line input-port) 0)])
    (cond
      [(equal? input #\+)     (token 'op #\+)]
      [(equal? input #\*)     (token 'op #\*)]
      [(equal? input #\()     (token 'lparen #\()]
      [(equal? input #\))     (token 'lparen #\))]
      [(equal? input #\0)     (token 'digit #\0)]
      [(equal? input #\1)     (token 'digit #\1)]
      [(equal? input #\2)     (token 'digit #\2)]
      [(equal? input #\3)     (token 'digit #\3)]
      [(equal? input #\4)     (token 'digit #\4)]
      [(equal? input #\5)     (token 'digit #\5)]
      [(equal? input #\6)     (token 'digit #\6)]
      [(equal? input #\7)     (token 'digit #\7)]
      [(equal? input #\8)     (token 'digit #\8)]
      [(equal? input #\9)     (token 'digit #\9)]
      [else (error "Invalid input. No such token exists")])))
      

; string -> 0 argument function that returns the next token on the string
; this function creates a function that uses get-next-token on the string that was passed in,
; notice how we pass create the input by using open-input-string
(define (lexstr str)
 (let ([input (open-input-string str)])
 (λ () (get-next-token input))))

; (() -> token) -> (ast-node | ast-expr-node)
; the parser takes a function (probably produced by lexstr) that
; lexes the contents of the input stream
;;(define (parser lex) ... )