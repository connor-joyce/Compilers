#lang racket

(require "environment.rkt"
         test-engine/racket-tests)

(check-expect (empty-env) `(, (make-hash)))

(check-expect (extend-env (empty-env) 'x 5) `(, (hash-copy #hash((x . 5)))))

(check-expect (apply-env (empty-env) 'x) #f)
(check-expect (apply-env (extend-env (empty-env)  'x 5) 'x) 5)

;;; Test pushing/poping scope

(check-expect (push-scope (empty-env)) `(, (make-hash) ,(make-hash)))
;;back quote (`) says dont' evaluate the next paren expression
;;the comma forces an evaluation of the next paren expression
(check-expect (pop-scope (push-scope (empty-env))) `(,(make-hash)))

(test)
