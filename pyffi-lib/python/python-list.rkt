#lang racket/base

(provide (all-defined-out))

(require "structs.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-types.rkt")

(require (for-syntax racket/base syntax/parse racket/syntax))

;;;
;;; Lists (Python lists)
;;;

(define (pylist? x)
  (and (obj? x)
       (or (equal? (obj-type-name x) "list")
           #;(PyList_Check (obj-the-obj x)))))

(define (pylist-new len)
  (unless (and (integer? len) (>= len 0))
    (raise-arguments-error 'pylist-new "expected a non-negative integer" "length" len))

  (define l (PyList_New len)) ; new reference
  (obj "list" l))

(define (pylist . xs)
  (list->pylist xs))

(define (list->pylist xs)
  (unless (list? xs)
    (raise-arguments-error 'list->pylist "expected a list" "xs" xs))
  (define n (length xs))
  (define l (PyList_New n))
  (for ([x (in-list xs)] [i (in-range n)])
    (define v (racket->python x))
    (case (PyList_SetItem l i v)
      [(0)  (void)] ; succes
      [(-1) (error 'list->pylist "some error 1")] ; out of range
      [else (error 'list->pylist "some error 2")]))
  (obj "list" l))

(define (vector->pylist xs)
  (define who 'vector->pylist)
  (unless (vector? xs)
    (raise-arguments-error who "expected a vector" "xs" xs))
  (define n (vector-length xs))
  (define l (PyList_New n))
  (for ([x (in-vector xs)] [i (in-range n)])
    (define v (racket->python x))
    (case (PyList_SetItem l i v)
      [(0)  (void)] ; succes
      [(-1) (error who "some error 1")] ; out of range
      [else (error who "some error 2")]))
  (obj "list" l))


(define (pylist-size x)
  (unless (pylist? x)
    (raise-arguments-error 'pylist-size "expected a pylist" "pylist" x))
  
  (define o (obj-the-obj x))
  (PyList_Size o))

(define (pylist-get-item pylist index)
  (unless (pylist? pylist)
    (raise-arguments-error 'pylist-get-item "expected a pylist" "pylist" pylist "index" index))
  (unless (and (integer? index) (>= index 0))
    (raise-arguments-error 'pylist-get-item
                           "expected a non-negative integer as index" "pylist" pylist "index" index))

  (define l (obj-the-obj pylist))
  (define v (PyList_GetItem l index))
  (when (eqv? v #f) (PyErr_Clear))
  (and v (python->racket v)))

(define (pylist->list pylist)
  (unless (pylist? pylist)
    (raise-arguments-error 'pylist->list "expected a pylist" "pylist" pylist))
  
  (define l (obj-the-obj pylist))
  (define n (PyList_Size l))
  (for/list ([i (in-range n)])
    (define v (PyList_GetItem l i))
    (when (eqv? v #f) (PyErr_Clear))
    (and v (pr v))))


(define (pylist-get-slice pylist low-index high-index)
  ; returns new pylist
  (unless (pylist? pylist)
    (raise-arguments-error 'pylist-get-slice
                           "expected a pylist" "pylist" pylist "low" low-index "high" high-index))
  (unless (and (integer? low-index) (>= low-index 0))
    (raise-arguments-error 'pylist-get-slice "expected a non-negative integer as the low index"
                           "pylist" pylist "low" low-index "high" high-index))
  (unless (and (integer? high-index) (>= high-index 0))
    (raise-arguments-error 'pylist-get-slice "expected a non-negative integer as the high index"
                           "pylist" pylist "low" low-index "high" high-index))

  (define l (obj-the-obj pylist))
  (define v (PyList_GetSlice l low-index high-index))
  (when (eqv? v #f) (PyErr_Clear))
  (and v (obj "list"  v)))

(define (pylist-set-slice! pylist low-index high-index item-pylist)
  (define who 'pylist-set-slice!)
  (unless (pylist? pylist)
    (raise-arguments-error who "expected a pylist"
                           "pylist" pylist "low" low-index "high" high-index "item-pylist" item-pylist))
  (unless (and (integer? low-index) (>= low-index 0))
    (raise-arguments-error who "expected a non-negative integer as the low index"
                           "pylist" pylist "low" low-index "high" high-index "item-pylist" item-pylist))
  (unless (and (integer? high-index) (>= high-index 0))
    (raise-arguments-error who "expected a non-negative integer as the high index"
                           "pylist" pylist "low" low-index "high" high-index "item-pylist" item-pylist))
  (unless (pylist? item-pylist)
    (raise-arguments-error who "expected a pylist"
                           "pylist" pylist "low" low-index "high" high-index "item-pylist" item-pylist))

  (define l (obj-the-obj pylist))
  (define i (obj-the-obj item-pylist))
  (define v (PyList_SetSlice l low-index high-index i))
  (when (eqv? v #f) (PyErr_Clear))
  (void))
    

(define (pylist-set-item! pylist index value)
  (unless (pylist? pylist)
    (raise-arguments-error 'pylist-set-item! "expected a pylist"
                           "pylist" pylist "index" index "value" value))
  (unless (and (integer? index) (>= index 0))
    (raise-arguments-error 'pylist-set-item!
                           "expected a non-negative integer as index"
                           "pylist" pylist "index" index "value" value))

  (define l (obj-the-obj pylist))
  (define v (racket->python value))

  (case (PyList_SetItem l index v)
    [(0)  (void)] ; succes
    [(-1) (raise-range-error 'pylist-set-item!
                             "pylist"
                             ""
                             index
                             pylist
                             0
                             (max 0 (- (pylist-size pylist) 1)))]
    [else (error 'pylist-set-item! "some error")]))

(define (pylist-insert! pylist index value)
  (unless (pylist? pylist)
    (raise-arguments-error 'pylist-insert! "expected a pylist"
                           "pylist" pylist "index" index "value" value))
  (unless (and (integer? index) (>= index 0))
    (raise-arguments-error 'pylist-insert!
                           "expected a non-negative integer as index"
                           "pylist" pylist "index" index "value" value))

  (define l (obj-the-obj pylist))
  (define v (racket->python value))

  (case (PyList_Insert l index v)
    [(0)  (void)] ; succes
    [(-1) (raise-range-error 'pylist-insert!
                             "pylist"
                             ""
                             index
                             pylist
                             0
                             (max 0 (- (pylist-size pylist) 1)))]
    [else (error 'pylist-insert! "some error")]))

(define (pylist-append-item! pylist item)
  (unless (pylist? pylist)
    (raise-arguments-error 'pylist-append-item! "expected a pylist" "pylist" pylist "item" item))
  
  (define ol (obj-the-obj pylist))
  (define oi (racket->python item))
  (case (PyList_Append ol oi)
    [(0) (void)]
    [else (PyErr_Clear)
          (error 'pylist-append-item! "an error occurred")]))

(define (pylist-reverse! pylist)
  (unless (pylist? pylist)
    (raise-arguments-error 'pylist-reverse! "expected a pylist" "pylist" pylist))
  
  (define ol (obj-the-obj pylist))
  (case (PyList_Reverse ol)
    [(0) (void)]
    [else (PyErr_Clear)
          (error 'pylist-reverse! "an error occurred")]))

(define (pylist-sort! pylist)
  (unless (pylist? pylist)
    (raise-arguments-error 'pylist-sort! "expected a pylist" "pylist" pylist))
  
  (define ol (obj-the-obj pylist))
  (case (PyList_Sort ol)
    [(0) (void)]
    [else (PyErr_Clear)
          (error 'pylist-sort! "an error occurred")]))

(define (pylist->tuple pylist)
  (define who 'pylist->tuple)
  (unless (pylist? pylist)
    (raise-arguments-error who "expected a pylist" "pylist" pylist))
  
  (define ol (obj-the-obj pylist))
  (define t (PyList_AsTuple ol))
  (case t
    [(#f) (PyErr_Clear)
          (error who "an error occurred")]
    [else (obj "tuple" t)]))


(define (in-pylist pylist)
  (define n (pylist-size pylist))
  (make-do-sequence
   (λ () (values (λ (pos) (pylist-get-item pylist pos))
                 #f
                 add1
                 0
                 (λ (pos) (< pos n))
                 #f
                 #f))))
