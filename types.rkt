#lang racket

(provide (all-defined-out))

;;; Types for the Ni language
;;The Guard is to determine whether or not a legitimate type is being created
;;Actual is for constructed types, if you make an array you also need to know what type the data in the array is.
;;And you're never allowed to create a NiType.
;;For non-constructed types '() is usually passed in as actual. 
(struct NiType ([actual #:mutable]) #:transparent
  #:guard (λ (actual typename)
            (if (eq? typename 'NiType)
                (error "Cannot Instantiate NiType directly")
                (if (or (eq? actual '())
                        (NiType? actual))
                    (values actual)
                    (raise-arguments-error typename "Can only instantiate with NiTypes or '()" "actual: " actual)))))

;;#:methods can redefine some of the default struct functions. Custom-write now prints out the type and thats it.
(struct StringType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:str" port)))])

(struct VoidType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                  (write-string "t:void" port)))])

(struct IntType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                  (write-string "t:int" port)))])

(struct BoolType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                  (write-string "t:bool" port)))])

(struct PengType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                  (write-string "t:peng" port)))])

(struct NameType NiType (name)#:transparent
  #:methods gen:custom-write [(define write-proc
                                (λ (ty port mode)
                                  (let ([theprinter (case mode
                                                      [(#t) write]
                                                      [(#f) display]
                                                      [else (λ (p port) (print p port mode))])])
                                    (write-string "<name: " port)
                                    (theprinter (NiType-actual ty) port)
                                    (write-string ">" port))))]
  #:guard (λ (actual name tyname)
            (cond
              [(and (not (symbol? name))
                    (raise-arguments-error tyname "NameType name must be a symbol"
                                           "actual" actual
                                           "name" name))]
              [else (values actual name)])))
;;Chris doesn't store the name inside of the struct
;;And I probably won't use the name field at all in implementation
(struct ArrayType NiType (name element-type
                               [label #:mutable #:auto]) #:transparent
  #:auto-value #f
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let
                                                       ([theprinter (case mode
                                                                      [(#t) write]
                                                                      [(#f) display]
                                                                      [else (λ (p port) (print p port mode))])])
                                                     (write-string "t:array[" port)
                                                     (theprinter (ArrayType-element-type ty) port)
                                                     (write-string "]" port))))])

(struct RecordType NiType (name fields [label #:mutable #:auto]) #:transparent
  #:auto-value #f
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (λ (p port) (print p port mode))])])
                                                     (display "t:rec{ " port)
                                                     (for-each (λ (field) (theprinter field port)
                                                                 (display " " port)) (RecordType-fields ty))
                                                     (display "}" port))))])

(struct NameTypePair (type name) #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t)    write]
                                                                       [(#f)    display]
                                                                       [else (λ (p port) (print p port mode))])])
                                                     (display "<" port)
                                                     (theprinter (NameTypePair-name ty) port)
                                                     (display ", " port)
                                                     (theprinter (NameTypePair-type ty) port)
                                                     (display ">" port))))]
  #:guard (λ (type name typename)
            (cond [(and (or (VarValue? type) (NiType? type)) (symbol? name))
                   (values type name)]
                  [else
                   (raise-arguments-error "NameTypePair requires a VarValue/NiType and symbol"
                                          typename
                                          "type" type
                                          "name" name)])))

(struct VarValue (type
                  [read-only? #:mutable]
                  [level #:mutable]
                  [escape? #:mutable]
                  [offset #:mutable]
                  [result #:auto #:mutable]) #:transparent #:auto-value #f)

(struct FunValue (name parameters return-type [label #:mutable #:auto]
                       [frame #:mutable #:auto])
  #:transparent #:auto-value #f)

(define (make-ArrayType name etype)
  (ArrayType '() name etype))

(define (make-RecordType name fields)
  (RecordType '() name fields))

(define (bind-nametype! nametype type)
  (set-NiType-actual! nametype type))

(define (make-VarValue ty)
  (VarValue ty #f #f #f #f))

;;functions for building types
(define (make-IntType)
  (IntType '()))

(define (make-BoolType)
  (BoolType '()))

(define (make-StringType)
  (StringType '()))

(define (make-PengType)
  (PengType '()))

(define (make-VoidType)
  (VoidType '()))

(define (actual-type ty)
  (let ([actual (NiType-actual ty)])
    (cond
      [(and (null? actual) (NameType? ty)) (error "NameType must have an actual by the time you call it")]
      [(null? actual) ty]
      [else (actual-type actual)])))




