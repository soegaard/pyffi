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

(define (dict? x)
  (and (obj? x)
       (or (equal? (obj-type-name x) "dict")
           (mapping? x))))

(define (dict-new)
  (define d (PyDict_New)) ; new reference
  (obj "dict" d))

(define (dict-proxy-new mapping)
  ; creates read-only dict
  (unless (mapping? mapping)
    (raise-arguments-error 'dict-proxy-new "expected a mapping" "mapping" mapping))

  (define o (obj-the-obj mapping))
  (define p (PyDictProxy_New o)) ; new reference
  (obj "mappingproxy" p))

(define (dict-clear! d) ; remove all keys
  (unless (dict? d)
    (raise-arguments-error 'dict-clear! "expected a dict" "dict" d))
  
  (define o (obj-the-obj d))
  (void (PyDict_Clear d)))

(define (dict-contains? dict key) ; equivalent to  "key in dict"
  (unless (dict? dict)
    (raise-arguments-error 'dict-contains? "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (define k (racket->python key))
  (case (PyDict_Contains d k)
    [(1)   #t]
    [(0)   #f]
    [else  (raise-arguments-error 'dict-contains? "an error occurred")]))

(define (dict-copy x)
  (unless (dict? x)
    (raise-arguments-error 'dict-copy "expected a dict" "dict" x))
  
  (define o (obj-the-obj x))
  (obj "dict" (PyDict_Copy o)))

(define (dict-set-item! dict key val)
  (unless (dict? dict)
    (raise-arguments-error 'dict-set-item! "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (define v (racket->python val))

  (cond
    [(string? key) ; fast path
     (case (PyDict_SetItemString d key v)
       [(0) (void)] ; succes
       [else (error 'dict-set-item "some error 1")])]
    [else
     (define k (racket->python key))
     (case (PyDict_SetItem d k v)
       [(0) (void)] ; succes
       [else (error 'dict-set-item "some error 2")])]))  ; todo

(define (dict-del-item! dict key)
  (unless (dict? dict)
    (raise-arguments-error 'dict-del-item! "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (cond
    [(string? key) ; fast path
     (case (PyDict_DelItemString d key)
       [(0) (void)] ; succes
       [else (error 'dict-del-item! "some error 1")])]
    [else
     (define k (racket->python key))
     (case (PyDict_DelItem d k)
       [(0) (void)] ; succes
       [else (error 'dict-del-item! "some error 2")])]))

(define (dict-get-item dict key)
  (unless (dict? dict)
    (raise-arguments-error 'dict-get-item "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (cond
    [(string? key) ; fast path
     (define v (PyDict_GetItemString d key)) ; never raises exceptions
     (and v (python->racket v))]     
    [else
     (define k (racket->python key))
     (define v (PyDict_GetItem d k))
     (and v (python->racket v))]))

(define (dict-keys x)
  (unless (dict? x)
    (raise-arguments-error 'dict-keys "expected a dict" "dict" x))
  
  (define o (obj-the-obj x))
  (define ks (PyDict_Keys o)) ; new reference
  (obj "list" ks))

(define (dict-values x)
  (unless (dict? x)
    (raise-arguments-error 'dict-values "expected a dict" "dict" x))
  
  (define o (obj-the-obj x))
  (define vs (PyDict_Values o)) ; new reference
  (obj "list" vs))

(define (dict-size x)
  (unless (dict? x)
    (raise-arguments-error 'dict-size "expected a dict" "dict" x))
  
  (define o (obj-the-obj x))
  (define s (PyDict_Size o)) ; new reference
  s)

(define (dict-merge! a b [override #t])
  (unless (dict? a)
    (raise-arguments-error 'dict-merge! "expected a dict as first argument" "a" a "b" b))
  (unless (mapping? b)
    (raise-arguments-error 'dict-merge! "expected a dict as second argument" "a" a "b" b))
  
  (define oa (obj-the-obj a))
  (define ob (obj-the-obj b))
  (case (PyDict_Merge oa ob (or (and override 1) 0))
    [(0) (void)]
    [else (error 'dict-merge! "an error occurred")])) ; todo

(define (dict-update! a b)
  (unless (dict? a)
    (raise-arguments-error 'dict-update! "expected a dict as first argument" "a" a "b" b))
  (unless (mapping? b)
    (raise-arguments-error 'dict-update! "expected a dict as second argument" "a" a "b" b))
  
  (define oa (obj-the-obj a))
  (define ob (obj-the-obj b))
  (case (PyDict_Update oa ob)
    [(0) (void)]
    [else (error 'dict-update! "an error occurred")]))

