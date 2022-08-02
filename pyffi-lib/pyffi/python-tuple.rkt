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

(define (pytuple? x)
  (and (obj? x)
       (equal? (obj-type-name x) "tuple")))

(define (pytuple-new len)
  (unless (and (integer? len) (>= len 0))
    (raise-arguments-error 'pytuple-new "expected a non-negative integer" "length" len))

  (define t (PyTuple_New len)) ; new reference
  (obj "tuple" t))

(define (list->pytuple xs)
  (unless (list? xs)
    (raise-arguments-error 'list->pytuple "expected a list" "xs" xs))
  (define n (length xs))
  (define t (PyTuple_New n))
  (for ([x (in-list xs)] [i (in-range n)])
    (define v (racket->python x))
    (Py_IncRef v) ; since Tuple_SetItem steals reference
    (case (PyTuple_SetItem t i v)
      [(0)  (void)] ; succes
      [(-1) (error 'list->pytuple "some error 1")] ; out of range
      [else (error 'list->pytuple "some error 2")]))
  (obj "tuple" t))

(define (pytuple . xs)
  (list->pytuple xs))

(define (iterable->pytuple o)
  (builtins.tuple o))

(define (vector->pytuple xs)
  (define who 'vector->pytuple)
  (unless (vector? xs)
    (raise-arguments-error who "expected a vector" "xs" xs))
  (define n (vector-length xs))
  (define t (PyTuple_New n))
  (for ([x (in-vector xs)] [i (in-range n)])
    (define v (racket->python x))
    (Py_IncRef v) ; since Tuple_SetItem steals reference
    (case (PyTuple_SetItem t i v)
      [(0)  (void)] ; succes
      [(-1) (error who "some error 1")] ; out of range
      [else (error who "some error 2")]))
  (obj "tuple" t))


(define (pytuple->vector xs)
  (define who 'pytuple->vector)
  (unless (pytuple? xs)
    (raise-arguments-error who "expected a pytuple" "xs" xs))

  (set! xs (obj-the-obj xs))
  (define n (PyTuple_Size xs))
  (define v (make-vector n))
  (for ([i (in-range n)])    
    (vector-set! v i (pr (PyTuple_GetItem xs i))))
  v)

(define (pytuple->list xs)
  (define who 'pytuple->list)
  (unless (pytuple? xs)
    (raise-arguments-error who "expected a pytuple" "xs" xs))

  (set! xs (obj-the-obj xs))
  (define n (PyTuple_Size xs))
  (for/list ([i (in-range n)])    
    (pr (PyTuple_GetItem xs i))))

(define (pytuple->pylist xs)
  (define who 'pytuple->pylist)
  (unless (pytuple? xs)
    (raise-arguments-error who "expected a pytuple" "xs" xs))

  (set! xs (obj-the-obj xs))
  (define n (PyTuple_Size xs))
  (define l (PyList_New n))
  (for ([i (in-range n)] )
    (define x (PyTuple_GetItem xs i))
    (Py_IncRef x) ; since PyList_SetItem steals reference
    (case (PyList_SetItem l i x)
      [(0)  (void)] ; succes
      [(-1) (error who "some error 1")] ; out of range
      [else (error who "some error 2")]))
  (obj "list" l))



(require (only-in racket/unsafe/ops unsafe-vector*->immutable-vector!))

(define (pytuple->immutable-vector xs)
  (define who 'pytuple->immutable-vector)
  (unless (pytuple? xs)
    (raise-arguments-error who "expected a pytuple" "xs" xs))
  
  (set! xs (obj-the-obj xs))
  (define n (PyTuple_Size xs))
  (define v (make-vector n))
  (for ([i (in-range n)])    
    (vector-set! v i (pr (PyTuple_GetItem xs i))))
  (unsafe-vector*->immutable-vector! v))


(define (pytuple-length x)
  (unless (pytuple? x)
    (raise-arguments-error 'pytuple-length "expected a pytuple" "tuple" x))
  
  (define o (obj-the-obj x))
  (PyTuple_Size o))

(define pytuple-size pytuple-length)

(define (pytuple-ref tuple index) ; Python: tuple.getitem(index)
  (unless (pytuple? tuple)
    (raise-arguments-error 'pytuple-ref "expected a pytuple" "tuple" tuple "index" index))
  (unless (and (integer? index) (>= index 0))
    (raise-arguments-error 'pytuple-ref
                           "expected a non-negative integer as index" "tuple" tuple "index" index))

  (define t (obj-the-obj tuple))
  (define v (PyTuple_GetItem t index))
  (when (eqv? v #f) (PyErr_Clear))
  (and v (python->racket v)))

(define (pytuple-get-slice tuple low-index high-index)
  ; returns new tuple
  (unless (pytuple? tuple)
    (raise-arguments-error 'tuple-get-slice
                           "expected a pytuple" "tuple" tuple "low" low-index "high" high-index))
  (unless (and (integer? low-index) (>= low-index 0))
    (raise-arguments-error 'pytuple-get-slice "expected a non-negative integer as the low index"
                           "tuple" tuple "low" low-index "high" high-index))
  (unless (and (integer? high-index) (>= high-index 0))
    (raise-arguments-error 'pytuple-get-slice "expected a non-negative integer as the high index"
                           "tuple" tuple "low" low-index "high" high-index))

  (define t (obj-the-obj tuple))
  (define v (PyTuple_GetSlice t low-index high-index))
  (when (eqv? v #f) (PyErr_Clear))
  (and v (obj "tuple"  v)))

(define (pytuple-set-item! tuple index value)
  (unless (pytuple? tuple)
    (raise-arguments-error 'pytuple-set-item! "expected a tuple"
                           "tuple" tuple "index" index "value" value))
  (unless (and (integer? index) (>= index 0))
    (raise-arguments-error 'pytuple-set-item!
                           "expected a non-negative integer as index"
                           "tuple" tuple "index" index "value" value))

  (define t (obj-the-obj tuple))
  (define v (racket->python value))
  (Py_IncRef v) ; since Tuple_SetItem steals reference
  (case (PyTuple_SetItem t index v)
    [(0)  (void)] ; succes
    [(-1) (raise-range-error 'pytuple-set-item!
                             "tuple"
                             ""
                             index
                             tuple
                             0
                             (max 0 (- (pytuple-size tuple) 1)))]
    [else (error 'pytuple-set-item! "some error")]))


(define (in-pytuple tuple)
  (define n (pytuple-size tuple))
  (make-do-sequence
   (λ () (values (λ (pos) (pytuple-ref tuple pos))
                 #f
                 add1
                 0
                 (λ (pos) (< pos n))
                 #f
                 #f))))



                 
