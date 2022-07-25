#lang racket/base
(provide := ref)

(require "structs.rkt"
         "python-c-api.rkt"
         "python-constants.rkt"
         "python-types.rkt"
         (only-in "python-builtins.rkt" slice-obj?)
         racket/format)

(require (for-syntax racket/base racket/syntax syntax/parse))

;;;
;;; operator - Standard operators as functions
;;;

; The Python module `operator` export a set of functions that correspond
; to the Python operators.

(define-syntax (define-binary-operator stx)
  (syntax-parse stx
    [(_define-binary-operators id:id)
     (with-syntax ([operator.id (format-id #'id "operator.~a" #'id)])
       (syntax/loc stx
         (begin
           (provide id)
           (define-py operator.id (~fun ~py ~py -> ~py))           
           (define (id x y) (operator.id x y)))))]))

(define-syntax-rule (define-binary-operators id ...)
  (begin (define-binary-operator id) ...))

(define-syntax (define-unary-operator stx)
  (syntax-parse stx
    [(_define-unary-operators id:id)
     (with-syntax ([operator.id (format-id #'id "operator.~a" #'id)])
       (syntax/loc stx
         (begin
           (provide id)
           (define-py operator.id (~fun ~py -> ~py))
           (define (id x) (operator.id x)))))]))

(define-syntax-rule (define-unary-operators id ...)
  (begin (define-unary-operator id) ...))


;;; Object Comparisons

; These operators can potentially return non-boolean results.
(define-binary-operators lt le eq ne ge gt)
             
;;; Logical operators
(define-unary-operators not_ truth is_ is_not)

;;; Mathematical and bitwise operators
;   Skipped: abs
(define-binary-operators
  add       ; a +  b
  floordiv  ; a // b
  and_      ; bitwise a and b
  index     ; convert to index using __index__()
  lshift    ; a shifted left by b
  mod       ; a % b
  mul       ; a * b
  matmul    ; a @ b
  or_       ; bitwise a or b
  pow       ; a ** b ,  a and b numbers
  rshift    ; a shifted right by b
  sub       ; a - b
  truediv   ; a/b where 2/3 is 0.66
  xor       ; bitwise a xor b  
  )

(define-unary-operators
  invert    ; bitwise invert
  neg       ; -a
  pos       ; +a  [sic]
  )

;;; Sequence operators

(define-binary-operators
  concat      ; a + b for a and b sequences
  contains    ; is b in a
  countOf     ; number of occurences of b in a
  delitem     ; remove value of a at index b
  getitem     ; return the value of a at index b
  indexOf     ; return index of the first occurence of b in a
  )

(define-unary-operators
  length_hint ; estimate length 
  )

;  setitem     ; set the value of a at index b to c
(define-py operator.setitem (~fun ~py ~py ~py -> ~py))
(define (setitem a b c) (operator.setitem a b c))

;;; Generalized attrinube and item lookup

; TODO

; operator.attrgetter(attr)
; operator.attrgetter(*attrs)

; operator.itemgetter(item)
; operator.itemgetter(*items)

; operator.methodcaller(name, /, *args, **kwargs)

; In-place Operators


(define-syntax (:= stx)
  (syntax-parse stx
    [(_ x:expr [i:expr] y:expr)
     (syntax/loc stx
       (operator.setitem x i y))]
    [(_ x:expr [i:expr j:expr] y:expr)
     (syntax/loc stx
       (let ([t (operator.getitem x i)])
         (operator.setitem t j y)))]
    [_ (error ':= "todo")]))


(define ref
  ; Note: a[e ...] is equivalent to a[(e,...)] where (e,...) is a tuple.
  (case-lambda
    [(seq i)
     (cond
       [(vector? seq)
        (vector-ref seq i)]
       [(integer? i)
        (define o  (if (obj? seq) (obj-the-obj seq) seq))
        (define v  (PyObject_GetItem o (integer->py-int i)))
        (python->racket v)]
       [(string? i)
        (define o  (if (obj? seq) (obj-the-obj seq) seq))
        (define v  (PyObject_GetItem o (string->py-string i)))
        (python->racket v)]
       [(slice-obj? i)
        (operator.getitem seq i)]
       [(vector? i) ; a vector (tuple) of indices
        (operator.getitem seq i)]
       [else (error 'ref (~a "got: " seq " " i))])]
    [(seq i j)
     (ref seq (vector i j))]
    [(seq i j k)
     (ref seq (vector i j k))]
    [(seq i j k l)
     (ref seq (vector i j k l))]
    [(seq i . is)
     (ref seq (list->vector is))]))
