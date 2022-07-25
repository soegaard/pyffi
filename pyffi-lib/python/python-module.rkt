#lang racket/base

(provide module?
         module-name
         module-dict
         module-dict-as-hash
         module-hash-ref
         module-submodules
         module-functions
         module-functions-with-signature)

(require "structs.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-types.rkt"
         "python-dict.rkt"
         "python-list.rkt"
         "python-functions.rkt")
(require (for-syntax racket/base syntax/parse racket/syntax))


(define (module? x)
  (and (obj? x)
       (member (obj-type-name x) '("module" "_automodule"))
       #t))

(define (module-name x)
  ; get the name of the module as a string
  (cond
    [(and (module? x)
          (PyModule_GetNameObject (obj-the-obj x)))
     => python->racket]
    [else #f]))

(define (module-dict x)
  ; get the `dict` (namespace) of the module
  (cond
    [(and (module? x)
          (PyModule_GetDict (obj-the-obj x)))
     => (λ (d) (obj "dict" d))]
    [else #f]))

(define (module-dict-as-hash x)
  ; get the `dict` (namespace) of the module
  (cond
    [(and (module? x)
          (PyModule_GetDict (obj-the-obj x)))
     => python->racket]
    [else #f]))
  

(define (module-hash-ref module-ht id [failure-result #f])
  (define str (or (and (string? id) id)
                  (and (symbol? id) (symbol->string id))
                  (error 'module-hash-ref "expected a string or symbol")))
  (hash-ref module-ht str failure-result))

(define (module-functions x)
  (define d  (module-dict x))
  (define ks (dict-keys   d))
  (define vs (dict-values d))
  (for/list ([k (in-pylist ks)]
             [v (in-pylist vs)]
             #:when (is-function? v))
    v))

(define (module-submodules x)
  (define d  (module-dict x))
  (define ks (dict-keys   d))
  (define vs (dict-values d))
  (for/list ([k (in-pylist ks)]
             [v (in-pylist vs)]
             #:when (is-module? v))
    v))

(define (module-functions-with-signature x)
  (define d  (module-dict x))
  (define ks (dict-keys   d))
  (define vs (dict-values d))
  (for/list ([k (in-pylist ks)]
             [v (in-pylist vs)]
             #:when (and (is-function? v)
                         (get-signature v)))
    v))



(define-syntax (define-functions stx)
  (syntax-parse stx
    [(_ qualifier:id id:id ...)
     (define qualifier-str   (symbol->string (syntax-e #'qualifier)))
     (define qualified-names (for/list ([id (syntax->list #'(id ...))])
                               (format-id id (string-append qualifier-str ".~a") id)))
     (with-syntax ([(qualified-name ...) qualified-names])
       (syntax/loc stx
         (define-delayed
           (define qualified-name
             (begin #;(displayln 'qualified-name)
                    (get-fun 'qualified-name)))
           ...)))]))

