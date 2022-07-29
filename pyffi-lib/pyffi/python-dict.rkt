#lang racket/base
(provide (all-defined-out))

(require "structs.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-types.rkt")

(require (for-syntax racket/base syntax/parse racket/syntax))

;;;
;;; Mapping Protocol
;;;

(define (mapping? x)
  (and (obj? x)
       (let ()
         (define o (obj-the-obj x))
         (define ok (PyMapping_Check o))
         (equal? ok 1))))

;;;
;;; Dict
;;;

(define (pydict? x)
  (and (obj? x)
       (or (equal? (obj-type-name x) "dict")
           (mapping? x))))

(define (pydict-new)
  (define d (PyDict_New)) ; new reference
  (obj "dict" d))

(define (pydict-proxy-new mapping)
  ; creates read-only dict
  (unless (mapping? mapping)
    (raise-arguments-error 'pydict-proxy-new "expected a mapping" "mapping" mapping))

  (define o (obj-the-obj mapping))
  (define p (PyDictProxy_New o)) ; new reference
  (obj "mappingproxy" p))

(define (pydict-clear! d) ; remove all keys
  (unless (pydict? d)
    (raise-arguments-error 'dict-clear! "expected a dict" "dict" d))
  
  (define o (obj-the-obj d))
  (void (PyDict_Clear d)))

(define (pydict-contains? dict key) ; equivalent to  "key in dict"
  (unless (pydict? dict)
    (raise-arguments-error 'pydict-contains? "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (define k (racket->python key))
  (case (PyDict_Contains d k)
    [(1)   #t]
    [(0)   #f]
    [else  (raise-arguments-error 'pydict-contains? "an error occurred")]))

(define (pydict-copy x)
  (unless (pydict? x)
    (raise-arguments-error 'pydict-copy "expected a dict" "dict" x))
  
  (define o (obj-the-obj x))
  (obj "dict" (PyDict_Copy o)))

(define (pydict-set! dict key val)
  (unless (pydict? dict)
    (raise-arguments-error 'pydict-set! "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (define v (racket->python val))

  (cond
    [(string? key) ; fast path
     (case (PyDict_SetItemString d key v)
       [(0) (void)] ; succes
       [else (error 'pydict-set! "some error 1")])]
    [else
     (define k (racket->python key))
     (case (PyDict_SetItem d k v)
       [(0) (void)] ; succes
       [else (error 'pydict-set! "some error 2")])]))  ; todo

(define (pydict-delete! dict key) ; delitem
  (unless (pydict? dict)
    (raise-arguments-error 'pydict-delete! "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (cond
    [(string? key) ; fast path
     (case (PyDict_DelItemString d key)
       [(0) (void)] ; succes
       [else (error 'pydict-delete! "some error 1")])]
    [else
     (define k (racket->python key))
     (case (PyDict_DelItem d k)
       [(0) (void)] ; succes
       [else (error 'pydict-delete! "some error 2")])]))

(define (pydict-ref dict key) ; dict.getitem()
  (unless (pydict? dict)
    (raise-arguments-error 'pydict-ref "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (cond
    [(string? key) ; fast path
     (define v (PyDict_GetItemString d key)) ; never raises exceptions
     (and v (python->racket v))]     
    [else
     (define k (racket->python key))
     (define v (PyDict_GetItem d k))
     (and v (python->racket v))]))

(define (pydict-keys x)
  (unless (pydict? x)
    (raise-arguments-error 'pydict-keys "expected a dict" "dict" x))
  
  (define o (obj-the-obj x))
  (define ks (PyDict_Keys o)) ; new reference
  (obj "list" ks))

(define (pydict-values x)
  (unless (pydict? x)
    (raise-arguments-error 'pydict-values "expected a dict" "dict" x))
  
  (define o (obj-the-obj x))
  (define vs (PyDict_Values o)) ; new reference
  (obj "list" vs))

(define (pydict-size x)
  (unless (pydict? x)
    (raise-arguments-error 'pydict-size "expected a dict" "dict" x))
  
  (define o (obj-the-obj x))
  (define s (PyDict_Size o)) ; new reference
  s)

(define (pydict-merge! a b [override #t])
  (unless (pydict? a)
    (raise-arguments-error 'pydict-merge! "expected a pydict as first argument" "a" a "b" b))
  (unless (mapping? b)
    (raise-arguments-error 'pydict-merge! "expected a pydict as second argument" "a" a "b" b))
  
  (define oa (obj-the-obj a))
  (define ob (obj-the-obj b))
  (case (PyDict_Merge oa ob (or (and override 1) 0))
    [(0) (void)]
    [else (error 'pydict-merge! "an error occurred")])) ; todo

(define (pydict-update! a b)
  (unless (pydict? a)
    (raise-arguments-error 'pydict-update! "expected a pydict as first argument" "a" a "b" b))
  (unless (mapping? b)
    (raise-arguments-error 'pydict-update! "expected a pydict as second argument" "a" a "b" b))
  
  (define oa (obj-the-obj a))
  (define ob (obj-the-obj b))
  (case (PyDict_Update oa ob)
    [(0) (void)]
    [else (error 'pydict-update! "an error occurred")]))

