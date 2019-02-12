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

(struct NameType NiType (name) #:transparent
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

;;missing record type, NameTypePair (for the fields of the record)

;;Values for Ni
;; VarValue (for the Named Variables)
;; FunValue (for named functions)



