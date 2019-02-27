#lang racket


;;this makes everything defined in this file available to any file
(provide (all-defined-out))

(require test-engine/racket-tests)

;;;Create a new empty environment
(define (empty-env)
  (list (make-hash)))
;;creates an empty hash table
;; '#hash() <--- hash literal


;;Extend an existing environment with a symbol and a value (symbol, value)
(define (extend-env env sym val)
  (hash-set! (first env) sym val)
  env)
;;hash-set! uses mutation to update the value in the hash table

;;Apply environment by using a symbol to find a value
;;Look up symbol in the hash table and return the associated value
(define (apply-env env sym)
  (cond
    [(eq? env '()) #f]
    [(hash-has-key? (first env) sym) (hash-ref (first env) sym)]
    [else (apply-env (rest env) sym)]))

(define (apply-env-scope env sym)
  (cond
    [(eq? env '()) #f]
    [(hash-has-key? (first env) sym) (hash-ref (first env) sym)]
    [else #f]))

;;push-scope
;;push a new scope (hash table) onto the environment
(define (push-scope env)
  (cons (make-hash) env))

;;pop-scope
;;removes a scope from the environment. Remove first hash table
(define (pop-scope env)
  (rest env))