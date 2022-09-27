#lang racket/base
(provide (all-defined-out))

(require "structs.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-types.rkt")

(require racket/match racket/format)

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
;;; Dictionaries (pydict)
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
  (void (PyDict_Clear o)))

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
  (define who 'pydict-set!)
  (unless (pydict? dict)
    (raise-arguments-error who "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (define v (racket->python val))

  (cond
    [(string? key) ; fast path
     (case (PyDict_SetItemString d key v)
       [(0) (void)] ; success
       [else (error who "error in call to PyDict_SetItemString")])]
    [else
     (define k (racket->python key))
     (case (PyDict_SetItem d k v)
       [(0) (void)] ; success
       [else (error who "error in call to PyDict_SetItem")])]))

(define (pydict-remove! dict key) ; delitem
  (define who 'pydict-remove!)
  (unless (pydict? dict)
    (raise-arguments-error who "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (cond
    [(string? key) ; fast path
     (case (PyDict_DelItemString d key)
       [(0) (void)] ; succes
       [else (PyErr_Clear) ; KeyError is thrown if item is not in the dict
             (void)])]
    [else
     (define k (racket->python key))
     (case (PyDict_DelItem d k)
       [(0) (void)] ; succes
       [else (PyErr_Clear) ; KeyError is thrown if item is not in the dict
             (void)])]))

(define (~w x)
  (let ([o (open-output-string)])
    (write x o)
    (get-output-string o)))


(define (pydict-ref dict key
                    [failure-result
                     (λ ()
                       (raise (make-exn:fail:contract
                               (~a "pydict-ref: no value found for key\n  key: " (~w key))
                               (current-continuation-marks))))])
  (unless (pydict? dict)
    (raise-arguments-error 'pydict-ref "expected a dict" "dict" dict))

  (define d (obj-the-obj dict))
  (cond
    [(string? key) ; fast path
     (define v (PyDict_GetItemString d key)) ; never raises exceptions
     (cond
       [v                           (pr v)]
       [(procedure? failure-result) (failure-result)]
       [else                        failure-result])]
    [else
     (define k (rp key))
     (define v (PyDict_GetItem d k))
     (cond
       [v                           (pr v)]
       [(procedure? failure-result) (failure-result)]
       [else                        failure-result])]))

(define (pydict->hash x
                      #:convert-key   [convert-key   pr/key]
                      #:convert-value [convert-value pr])
  (define o  (obj-the-obj x))
  (define vs (PyDict_Keys o)) ; pylist
  (define n  (PyList_Size vs))
  (for/hash ([i (in-range n)])
    (define k (PyList_GetItem vs i))
    (when (eqv? k #f) (PyErr_Clear))
    (define key (and k (convert-key k)))
    
    (define v (and k (PyDict_GetItem o k)))
    (when (and k (eqv? v #f)) (PyErr_Clear))
    (define val (and k v (convert-value v)))
    
    (values key val)))

(define (hash->pydict x #:convert [convert rp])
  (define who 'hash->pydict)
  (unless (hash? x)
    (raise-arguments-error who "expected a hash table" "hash" x))
  
  (define d (PyDict_New))
  (for ([(k v) (in-hash x)])
    (cond
      [(string? k) ; fast path
       (case (PyDict_SetItemString d k (convert v))
         [(0) (void)] ; succes
         [else (error who "error during call to PyDict_SetItemString")])]
      [else
     (case (PyDict_SetItem d (convert k) (convert v))
       [(0) (void)] ; succes
       [else (error who "error during call to PyDict_SetItem")])]))
  (obj "dict" d))

(define (pydict #:convert [convert rp] . args)
  (define who 'pydict)
  (define n (length args))
  (unless (even? n)
    (raise-arguments-error who "expected an even number of arguments" "keys and values" args))

  (define d (PyDict_New)) ; new reference
  
  (let loop ([as args])
    (match as
      ['() (void)]
      [(list* k v as)
       (define val (convert v))
       (cond
         [(string? k) ; fast path
          (case (PyDict_SetItemString d k val)
            [(0) (void)] ; succes
            [else (error who "error during call to PyDict_SetItemString")])]
         [else
          (define key (convert k))
          (case (PyDict_SetItem d key val)
            [(0) (void)] ; succes
            [else (error who "error during call to PyDict_SetItem")])])
       (loop as)]))
  (obj "dict" d))


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

(define (pydict-count x)
  (define who 'pydict-count)
  (unless (pydict? x)
    (raise-arguments-error who "expected a dict" "dict" x))
  
  (define o (obj-the-obj x))
  (define s (PyDict_Size o)) ; new reference
  s)

(define (pydict-merge! a b [override #t])
  (define who 'pydict-merge!)
  (unless (pydict? a)
    (raise-arguments-error who "expected a pydict as first argument" "a" a "b" b))
  (unless (mapping? b)
    (raise-arguments-error who "expected a pydict as second argument" "a" a "b" b))
  
  (define oa (obj-the-obj a))
  (define ob (obj-the-obj b))
  (case (PyDict_Merge oa ob (or (and override 1) 0))
    [(0) (void)]
    [else (error who "an error occurred")])) ; todo

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

(require (only-in ffi/unsafe malloc memset free))

; The position is a pointer to an size_t integer initialized to zero.

(define (pydict->key/values x)
  (define who 'pydict->key/values)
  (unless (pydict? x)
    (raise-arguments-error who "expected a pydict as argument" "d" x))

  (define o (obj-the-obj x))
  ; Allocate new position
  (define pos (malloc 'raw 16))
  ; Initialize position to zero.
  (memset pos 0 16)
  ; loop until pos is zero again
  (let loop ([kvs '()])
    (define flag/key/value (PyDict_Next o pos #f #f))
    (case (car flag/key/value)
      [(0) (free pos)
           (reverse kvs)]
      [else (define kv  (list (pr (cadr  flag/key/value))       
                              (pr (caddr flag/key/value))))
            (loop (cons kv kvs))])))

(define (in-pydict pydict)
  (define o (obj-the-obj pydict))
  (make-do-sequence
   (λ () (values (λ (pos) (values (pr (caddr pos)) (pr (cadddr pos))))      ; pos->elm
                 #f                                                         ; update (identity)
                 (λ (pos) (cons (car pos) (PyDict_Next o (car pos) #f #f))) ; next
                 (let ([pos (malloc 'raw 16)])                              ; initial-pos 
                   (memset pos 0 16)
                   (cons pos (PyDict_Next o pos #f #f)))
                 (λ (pos)                                                   ; continue with pos?
                   (define cont? (not (= (cadr pos) 0)))
                   (unless cont? (free (car pos)))
                   cont?)                                        
                 #f
                 #f))))
