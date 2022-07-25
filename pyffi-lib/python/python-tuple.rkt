#lang racket/base
(provide (all-defined-out))

(require "structs.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         (only-in "python-builtins.rkt" builtins.tuple)
         "python-types.rkt")

(require (for-syntax racket/base syntax/parse racket/syntax))

;;;
;;; Tuples
;;;

(define (tuple? x)
  (and (obj? x)
       (equal? (obj-type-name x) "tuple")))

(define (tuple-new len)
  (unless (and (integer? len) (>= len 0))
    (raise-arguments-error 'tuple-new "expected a non-negative integer" "length" len))

  (define t (PyTuple_New len)) ; new reference
  (obj "tuple" t))

(define (list->tuple xs)
  (unless (list? xs)
    (raise-arguments-error 'list->tuple "expected a list" "xs" xs))
  (define n (length xs))
  (define t (PyTuple_New n))
  (for ([x (in-list xs)] [i (in-range n)])
    (define v (racket->python x))
    (case (PyTuple_SetItem t i v)
      [(0)  (void)] ; succes
      [(-1) (error 'list->tuple "some error 1")] ; out of range
      [else (error 'list->tuple "some error 2")]))
  (obj "tuple" t))

(define (tuple . xs)
  (list->tuple xs))

(define (iterable->tuple o)
  (builtins.tuple o))

(define (vector->tuple xs)
  (define who 'vector->tuple)
  (unless (vector? xs)
    (raise-arguments-error who "expected a vector" "xs" xs))
  (define n (vector-length xs))
  (define t (PyTuple_New n))
  (for ([x (in-vector xs)] [i (in-range n)])
    (define v (racket->python x))
    (case (PyTuple_SetItem t i v)
      [(0)  (void)] ; succes
      [(-1) (error who "some error 1")] ; out of range
      [else (error who "some error 2")]))
  (obj "tuple" t))


(define (tuple-size x)
  (unless (tuple? x)
    (raise-arguments-error 'tuple-size "expected a tuple" "tuple" x))
  
  (define o (obj-the-obj x))
  (PyTuple_Size o))

(define (tuple-get-item tuple index)
  (unless (tuple? tuple)
    (raise-arguments-error 'tuple-get-item "expected a tuple" "tuple" tuple "index" index))
  (unless (and (integer? index) (>= index 0))
    (raise-arguments-error 'tuple-get-item
                           "expected a non-negative integer as index" "tuple" tuple "index" index))

  (define t (obj-the-obj tuple))
  (define v (PyTuple_GetItem t index))
  (when (eqv? v #f) (PyErr_Clear))
  (and v (python->racket v)))

(define (tuple-get-slice tuple low-index high-index)
  ; returns new tuple
  (unless (tuple? tuple)
    (raise-arguments-error 'tuple-get-slice
                           "expected a tuple" "tuple" tuple "low" low-index "high" high-index))
  (unless (and (integer? low-index) (>= low-index 0))
    (raise-arguments-error 'tuple-get-slice "expected a non-negative integer as the low index"
                           "tuple" tuple "low" low-index "high" high-index))
  (unless (and (integer? high-index) (>= high-index 0))
    (raise-arguments-error 'tuple-get-slice "expected a non-negative integer as the high index"
                           "tuple" tuple "low" low-index "high" high-index))

  (define t (obj-the-obj tuple))
  (define v (PyTuple_GetSlice t low-index high-index))
  (when (eqv? v #f) (PyErr_Clear))
  (and v (obj "tuple"  v)))

(define (tuple-set-item! tuple index value)
  (unless (tuple? tuple)
    (raise-arguments-error 'tuple-set-item! "expected a tuple"
                           "tuple" tuple "index" index "value" value))
  (unless (and (integer? index) (>= index 0))
    (raise-arguments-error 'tuple-set-item!
                           "expected a non-negative integer as index"
                           "tuple" tuple "index" index "value" value))

  (define t (obj-the-obj tuple))
  (define v (racket->python value))

  (case (PyTuple_SetItem t index v)
    [(0)  (void)] ; succes
    [(-1) (raise-range-error 'tuple-set-item!
                             "tuple"
                             ""
                             index
                             tuple
                             0
                             (max 0 (- (tuple-size tuple) 1)))]
    [else (error 'tuple-set-item! "some error")]))


(define (in-tuple tuple)
  (define n (tuple-size tuple))
  (make-do-sequence
   (λ () (values (λ (pos) (tuple-get-item tuple pos))
                 #f
                 add1
                 0
                 (λ (pos) (< pos n))
                 #f
                 #f))))



                 
