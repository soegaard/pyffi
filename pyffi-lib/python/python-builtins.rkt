#lang racket/base
;;;
;;; Builtins
;;;

; This module provides bindings for the functions in the Python module `builtins`.

(require "structs.rkt"
         "parameters.rkt"
         "python-c-api.rkt"
         "python-constants.rkt"
         "python-delayed.rkt"
         ; (rename-in (except-in "python-types.rkt" ~py) [~PY ~py])
         "python-types.rkt"
         
         racket/format)

; (define-py builtins.repr  (~fun ~obj -> ~string))

(define-py aiter          (~fun ~py -> ~py)      #:from builtins)
(define-py all            (~fun ~py -> ~bool)    #:from builtins)
; anext
(define-py any            (~fun ~py -> ~bool)    #:from builtins)
(define-py ascii          (~fun ~py -> ~string)  #:from builtins)
(define-py bin            (~fun ~py -> ~string)  #:from builtins)
(define-py bool           (~fun ~py -> ~bool)    #:from builtins)    ; todo: arg is optional
; breakpoint (not relevant in an embedding)
(define-py bytearray      (~fun ~py ~py ~py -> ~obj #:first-optional 0)  #:from builtins)
(define-py bytes          (~fun ~py ~py ~py -> ~obj #:first-optional 0)  #:from builtins) ; note, todo nameclash
(define-py callable       (~fun ~py -> ~bool)    #:from builtins)
(define-py chr            (~fun ~int -> ~string) #:from builtins)
; @classmethod
; compile
(define-py complex        (~fun ~py ~py -> ~obj #:first-optional 0) #:from builtins) ; complex class
(define-py delattr        (~fun ~obj ~string -> ~None) #:from builtins)

(provide dict) 
(define (dict x)
  (cond
    [(hash? x) (define d (PyDict_New))
               (for ([(k v) (in-hash x)])
                 (PyDict_SetItem d (racket->python k) (racket->python v)))
               (obj "dict" d)]
    [else (error 'dict (~a "expected a hash, got: " x))]))

; dir is in "python-more-builtins.rkt"
; (define-py dir           (~fun ~py -> ~py #;~list #:first-optional 0) #:from builtins)

(define-py divmod        (~fun ~py ~py -> ~py) #:from builtins)
(define-py enumerate     (~fun ~py #:start ~int -> ~obj) #:from builtins)
; (define-py builtins.eval (~fun ~py ~py ~py #:first-optional 1 -> ~py))  ; ?
; exec
; (define-py builtins.filter (~fun ~py ~py -> ~obj)) ; todo: callbacks ?
; filter
(define-py builtins.float   (~fun ~py         -> ~float  #:first-optional 0))
(define-py builtins.format  (~fun ~py ~py     -> ~py     #:first-optional 1))
(define-py frozenset        (~fun ~py         -> ~py     #:first-optional 0) #:from builtins)
(define-py getattr          (~fun ~py ~py ~py -> ~py     #:first-optional 2) #:from builtins)
;(define-py builtins.globals (~fun -> ~obj)) ; todo
(define-py hasattr          (~fun ~py ~py     -> ~bool) #:from builtins)
(define-py builtins.hash    (~fun ~py -> ~int))
; help  ; for interactive use
(define-py hex              (~fun ~py -> ~py) #:from builtins)
(define-py builtins.id      (~fun ~py -> ~py))
; input ; for interactive use
(define-py builtins.int     (~fun ~py #:base ~int -> ~int  #:first-optional 0))
(define-py isinstance       (~fun ~py ~py -> ~py)                     #:from builtins)
(define-py issubclass       (~fun ~py ~py -> ~py)                     #:from builtins)
(define-py iter             (~fun ~py ~py -> ~py  #:first-optional 1) #:from builtins)
(define-py len              (~fun ~py     -> ~int)                    #:from builtins)
(define-py builtins.list    (~fun ~py     -> ~py  #:first-optional 0))
;(define-py builtins.locals  (~fun -> ~obj)) ; todo
; map ; todo:  callback, callable
(define-py builtins.max     (~fun ~py     -> ~py  #:first-optional 0))
; max         ; todo
; memoryview  ; todo
; min         ; todo
(define-py next             (~fun ~py ~py -> ~py #:first-optional 1) #:from builtins)
(define-py object           ~obj #:from builtins)
(define-py oct              (~fun ~py -> ~py) #:from builtins)
; open        ; todo file related
(define-py ord              (~fun ~char -> ~int) #:from builtins)
(define-py builtins.pow     (~fun ~py ~py ~py -> ~py  #:first-optional 2))
; Note: The two argument version of pow is equivalent to operator.pow i.e. to base**exp
; Note: The operator version is given the name pow.
; print      ; todo i/o
; property   ; todo
(define-py builtins.range  (~fun ~py ~py ~py -> ~py #:first-optional 1)) ; todo: use ~class
(define-py repr            (~fun ~py -> ~string)  #:from builtins)
(add-initialization-thunk (λ () (current-repr repr)))
(define-py reversed        (~fun ~py -> ~py) #:from builtins)
(define-py builtins.round  (~fun ~py ~py -> ~py #:first-optional 1))
(define-py builtins.set    (~fun ~py -> ~py #:first-optional 0))         ; todo: use ~class
;(define-py setattr         (~fun ~py ~py ~py -> ~py) #:from builtins)
(define-py builtins.slice  (~fun ~py ~py ~py -> ~py))
(provide slice-obj?)
(define (slice-obj? x)
  (and (obj? x) (equal? (obj-type-name x) "slice")))

(define-py sorted          (~fun ~py #:key ~py #:reverse ~py -> ~list) #:from builtins)
; staticmethod  ; todo
(define-py str             (~fun ~py ~py ~py -> ~py #:first-optional 1) #:from builtins) ; todo use ~class
(add-initialization-thunk (λ () (current-str str)))
(define-py sum             (~fun ~py #:start ~py -> ~py) #:from builtins)
; super ; todo
(define-py builtins.tuple  (~fun ~py -> ~py #:first-optional 0) )  ; todo: use ~class

; type        ; todo
(define-py vars           (~fun ~py -> ~py #:first-optional 0) #:from builtins)
; zip         ; todo
; __import__  ; todo
