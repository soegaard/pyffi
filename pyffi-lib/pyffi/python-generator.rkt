#lang racket/base

(provide (all-defined-out))

(require "structs.rkt"
         "parameters.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-types.rkt"
         "python-attributes.rkt")

(require (for-syntax racket/base syntax/parse racket/syntax))


(define (pygenerator? x)
  (and (obj? x)
       (or (equal? (obj-type-name x) "generator")
           #;(PyList_Check (obj-the-obj x)))))

(define none-yet (list 'none-yet))

(define (in-pygenerator pygen)
  (let ([next-val none-yet])
    (make-do-sequence
     (λ () (values (λ (pos) ; pos->element
                     (begin0
                         (if (eq? next-val none-yet)
                             (pygen .__next__)
                             next-val)
                       (set! next-val (pygen .__next__))))
                   #f                          ; optional early-next-pos
                   (λ (pos) pos)               ; next-pos
                   pygen                       ; initial pos
                   (λ (pos) (not (eq? next-val 'StopIteration)))  ; continue with pos
                   #f
                   #f)))))


(current-pygenerator-prop:sequence in-pygenerator)
