#lang racket/base
(provide (all-defined-out))

(require "python-types.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-evaluation.rkt"
         "python-constants.rkt"
         "python-builtins.rkt"
         "python-operators.rkt"
         "structs.rkt"
         racket/format)

(require (only-in ffi/unsafe ->))
(require (for-syntax (only-in ffi/unsafe ->)
                     racket/base racket/syntax syntax/parse))

;;;
;;; Numpy
;;;

; Use (initialize-numpy) before using functions exported here.

;;;
;;; Initialization
;;;

; We need to:
;  1. Import numpy into the main module
;  2. Extract values such as dtypes from Numpy.

(provide initialize-numpy)
(define (initialize-numpy)
  (import-numpy)
  (initialize-dtypes)
  (void (run* "import operator as operator")))


(define (numpy-scalar->number x) (tell x item -> ~int))

; Use as return type only:
(define ~numpy-scalar (pytype "numpyscalar" (λ (x) (error 'todo)) numpy-scalar->number))

(define ~ndarray  (pytype "ndarray" ndarray->py-ndarray py-ndarray->ndarray))
; a shape is an integer or a tuple of integers
(define ~shape    (pytype "shape"
                          (λ (x)
                            (cond
                              [(integer? x) (integer->py-int x)]
                              [(vector? x)  (vector->py-tuple x)]
                              [else (error '~shape (~a "expected an integer or a vector of integers, got: " x))]))
                          (λ (x)
                            (case (python-type x)
                              [("int")   (py-int->number x)]
                              [("tuple") (py-tuple->vector x)]
                              [else (error '~shape (~a "expected an integer or a tuple of integers, got: " x))]))))
                                             

(define ~dtype (pytype "dtype" obj-the-obj #f))
(define ~order (pytype "order" string->py-string py-string->string))
                               
(define-syntax (define-dtypes stx)
  (syntax-parse stx
    [(_define-dtypes init-name name:id ...)
     (define names (for/list ([name (syntax->list #'(name ...))])
                     (format-id name "numpy.~a" name)))
     (with-syntax ([(numpy.name ...) names])
       (syntax/loc stx
         (begin
           (provide name) ...
           (define name #f) ...
           (define (init-name)
             (set! name (obj (~a"dtype<" 'name ">") (get 'numpy.name))) ...))))]))

(define-dtypes initialize-dtypes
  ; Note: float96 and complex192 not availabe on my system
  bool8
  int8   int16  int32  int64
  uint8 uint16 uint32 uint64
  intp uintp
  float16 float32 float64 float128
  complex64 complex128  complex256
  double longdouble
  csingle cdouble clongdouble)

;;;
;;; Attributes of `ndarray`
;;;

(define (.T        A) (getattr A "T"))
(define (.data     A) (getattr A "data"))
(define (.dtype    A) (getattr A "dtype"))
(define (.flags    A) (getattr A "flags"))
(define (.flat     A) (getattr A "flat"))
(define (.imag     A) (getattr A "imag"))
(define (.real     A) (getattr A "real"))
(define (.size     A) (getattr A "size"))
(define (.itemsize A) (getattr A "itemsize"))
(define (.nbytes   A) (getattr A "nbytes"))
(define (.ndim     A) (getattr A "ndim"))
(define (.shape    A) (getattr A "shape"))
(define (.strides  A) (getattr A "strides"))
(define (.ctypes   A) (getattr A "ctypes"))
(define (.base     A) (getattr A "base"))

;;;
;;; Constructors
;;;

; numpy.array(object, dtype=None, *, copy=True, order='K', subok=False, ndmin=0, like=None)
(define-py numpy.array (~fun ~list #:dtype ~dtype -> ~ndarray)) ; todo

; low-level constructor - the docs recommend using `array`, `zeros` or `empty` 
(define-py numpy.ndarray (~fun #:shape   ~tuple  ; shape
                               #:dtype   ~dtype  ; default is float
                               #:buffer  ~obj    ; object with buffer interface
                               #:offset  ~int    ; offset of array data in buffer
                               #:strides ~tuple  ; tuple of ints
                               #:order   ~char   ; "C" or "F"
                               -> ~ndarray))

;; ;;;
;; ;;; Methods of `ndarray`
;; ;;;

#;(define-py numpy.all (~fun ~py
                           #:axis     ~py      ; None or int or tuple of ints, optional, default None
                           #:out      ~ndarray
                           #:keepdims ~bool
                           #:where    ~py      ; array-like
                           -> ~py))
#;(define-py numpy.any (~fun ~py
                           #:axis     ~py      ; None or int or tuple of ints, optional, default None
                           #:out      ~ndarray
                           #:keepdims ~bool
                           #:where    ~py      ; array-like
                           -> ~py))
; numpy.arange([start, ]stop, [step, ]dtype=None, *, like=None)
(define-py numpy.arange (~fun ~py ~py ~py #:dtype ~dtype #:like ~py -> ~ndarray
                              #:first-optional 1))
; ndarray.argmax(a, axis=None, out=None, *, keepdims=False)
#;(define-py numpy.argmax (~fun ~py
                              #:axis     ~py      
                              #:out      ~ndarray
                              #:keepdims ~bool
                              -> ~py))
#;(define-py numpy.argmin (~fun ~py
                              #:axis     ~py      
                              #:out      ~ndarray
                              #:keepdims ~bool
                              -> ~py))
; ndarray.argpartition(a, kth, axis=- 1, kind='introselect', order=None)
#;(define-py numpy.argpartition (~fun ~py           ; a
                                    ~py           ; kth
                                    #:axis  ~py      
                                    #:kind  ~ndarray
                                    #:order ~bool
                                    -> ~py))
; ndarray.argsort(a, axis=- 1, kind=None, order=None)
#;(define-py numpy.argsort (~fun ~py           ; a
                               #:axis  ~py      
                               #:kind  ~ndarray
                               #:order ~bool
                               -> ~py))
; ndarray.astype(a, dtype, order='K', casting='unsafe', subok=True, copy=True)
(define-py numpy.ndarray.astype (~fun ~py               ; a
                                      ~dtype
                                      #:order   ~char   ; default "K"
                                      #:casting ~string ; default "unsafe"
                                      #:subok   ~bool   ; default #t
                                      #:copy    ~bool   ; default #t
                                      -> ~ndarray))
; ndarray.byteswap(a, inplace=False)
(define-py numpy.ndarray.byteswap (~fun ~py               ; a
                                        #:inplace ~bool   ; default #f
                                        -> ~py)) 
; ndarray.choose(a, choices, out=None, mode='raise')
#;(define-py numpy.choose (~fun ~py               ; a
                              ~py               ;
                              #:out ~py
                              #:mode ~py
                              -> ~py))
; ndarray.clip(a, min=None, max=None, out=None, **kwargs)
#;(define-py numpy.clip (~fun ~py ~py ~py
                            #:out ~py
                            ; todo: **kwargs ?
                            -> ~py))

;; >>> inspect.signature(numpy.clip)
;; <Signature (a, a_min, a_max, out=None, **kwargs)>
;; >>> inspect.getfullargspec(numpy.clip)
;; FullArgSpec(args=[], varargs='args', varkw='kwargs', defaults=None, kwonlyargs=[], kwonlydefaults=None, annotations={})


; ndarray.compress(a, condition, axis=None, out=None)
#;(define-py numpy.compress (~fun ~py               ; a
                                ~py               ;
                                #:axis ~py
                                #:out  ~py
                                -> ~py))
; numpy.diag(a, v, k=0)[source]
#;(define-py numpy.diag (~fun ~py
                            #:axis ~py
                            #:out  ~py
                            -> ~py))
; numpy.full(shape, fill_value, dtype=None, order='C', *, like=None)
#;(define-py numpy.full (~fun ~py ~py
                            #:dtype ~py
                            #:order ~py
                            #:like ~py                            
                            -> ~py))
; numpy.interp(x, xp, fp, left=None, right=None, period=None)
#;(define-py numpy.interp (~fun ~py ~py ~py
                              #:left   ~py
                              #:right  ~py
                              #:period ~py                            
                              -> ~py))
; numpy.isnan(x, /, out=None, *, where=True, casting='same_kind', order='K',
;              dtype=None, subok=True[, signature, extobj]) = <ufunc 'isnan'>
#;(define-py numpy.isnan (~fun ~py
                             #:out ~py
                             #:where ~py
                             #:casting ~py
                             #:order ~py
                             #:dtype ~py
                             #:subok ~py
                             -> ~py))
; randn ; todo generalize
(define-py numpy.random.randn (~fun ~py -> ~py))


; numpy.zeros(shape, dtype=float, order='C', *, like=None)
; Return a new array of given shape and type, filled with zeros.
(define-py numpy.zeros          (~fun ~shape #:dtype ~dtype -> ~ndarray))

(define-py numpy.ndarray.tolist (~fun ~ndarray -> ~py))
; (define-py numpy.ndarray.item   (~fun ~py      -> ~py))

;;; Numpy - Array Creation

(define-py numpy.empty (~fun ~shape #:dtype ~dtype #:order ~order -> ~ndarray))

#;(define-py numpy.eye   (~fun ~int #:dtype ~dtype #:order ~order -> ~ndarray))
#;(define-py numpy.shape (~fun ~ndarray -> ~tuple))

#;(define-py numpy.multiply (~fun ~py ~py -> ~py))

; numpy.count_nonzero(a, axis=None, *, keepdims=False)
#;(define-py numpy.count_nonzero (~fun ~py #:axis ~py #:keepdims ~py -> ~py))


;;;
;;; Numpy `numpy.random`
;;;

; random.seed(self, seed=None)
(define-py numpy.random.seed   (~fun ~py -> ~py))
; random.choice(a, size=None, replace=True, p=None)
(define-py numpy.random.choice (~fun ~py #:size ~py #:replace ~py #:p ~py -> ~py))


;;;
;;; Numpy FFT
;;;

; From the `numpy.fft` module

; fft.fft(a, n=None, axis=- 1, norm=None)
#;(define-py numpy.fft.fft (~fun ~py               ; a
                               #:n    ~py
                               #:axis ~py
                               #:norm ~py
                               -> ~py))
