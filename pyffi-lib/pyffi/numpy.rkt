#lang racket/base
(require "structs.rkt"
         "python-c-api.rkt"
         "python-delayed.rkt"
         "python-define-delayed.rkt"
         "python-environment.rkt"
         "python-evaluation.rkt"
         "python-initialization.rkt"
         
         "python-types.rkt"
         (except-in "python-attributes.rkt" #%app #%top)
         "python-builtins.rkt"
         "python-functions.rkt"
         "python-operators.rkt"

         "python-list.rkt"
         
         racket/format
         racket/list
         racket/match)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(require (only-in "numpy-core.rkt"
                  numpy.array numpy.ndarray numpy.ndarray.tolist numpy-scalar->number numpy.arange
                  numpy.random.choice
                  numpy.zeros
                  initialize-numpy))
(provide numpy.arange numpy.array numpy.ndarray numpy.ndarray.tolist numpy.zeros numpy.random.choice)
(provide initialize-numpy ref numpy-scalar->number)


;;;
;;; Result values
;;;

;; Functions from `numpy` can return arrays or scalars.
;; When scalars are returned, the custom `numpy` scalar types are used.
;; A Numpy scalar behaves like an array with one element, which
;; can be accessed with the method `item`.

(provide from)
(define (from A)
  (define s (getattr A "shape"))
  (match s
    [(vector)       (tell A item -> ~py)]
    [(vector n)     (pylist->list (numpy.ndarray.tolist A))]
    [(vector m n)   (for/list ([i m])
                      (pylist->list (numpy.ndarray.tolist (ref A i))))]
    [(vector m n o) (for/list ([i m])
                      (define Ai (ref A i))
                      (for/list ([j n])
                        (pylist->list (numpy.ndarray.tolist (ref Ai j)))))]
    ; todo: handle deeper nested arrays here
    [_
     (error 'from-array "todo")]))


;;;
;;; Numpy functions with a signature
;;;

;; Functions that `inspect.signature` works on can be automatically imported using `get-fun`.
;; The script "signature.py" prints the list of these functions/

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
             (get-fun 'qualified-name))
           ...)))]))

(define-functions numpy
  ; deprecated and removed in latest numpy:
  ;   alen asscalar
  __dir__ __getattr__ add_newdoc #;alen all allclose alltrue amax amin
  angle any append apply_along_axis apply_over_axes argmax argmin
  argpartition argsort argwhere around array2string array_equal
  array_equiv array_repr array_split array_str asarray_chkfinite
  asfarray asmatrix #;asscalar atleast_1d atleast_2d atleast_3d average
  bartlett base_repr binary_repr blackman block bmat broadcast_arrays
  broadcast_shapes broadcast_to byte_bounds choose clip column_stack
  common_type compress convolve copy corrcoef correlate count_nonzero
  cov cross cumprod cumproduct cumsum delete deprecate
  deprecate_with_doc diag diag_indices diag_indices_from diagflat
  diagonal diff digitize disp dsplit dstack ediff1d einsum einsum_path
  expand_dims extract eye fill_diagonal find_common_type fix flatnonzero
  flip fliplr flipud format_float_positional format_float_scientific
  fromfunction fromregex full full_like genfromtxt geomspace
  get_array_wrap get_include get_printoptions getbufsize geterr
  geterrcall gradient hamming hanning histogram histogram2d
  histogram_bin_edges histogramdd hsplit hstack i0 identity imag in1d
  indices info insert interp intersect1d isclose iscomplex iscomplexobj
  isfortran isin isneginf isposinf isreal isrealobj isscalar issctype
  issubclass_ issubdtype issubsctype iterable ix_ kaiser kron linspace
  load loadtxt logspace lookfor mask_indices mat max maximum_sctype mean
  median meshgrid min mintypecode moveaxis msort nan_to_num nanargmax
  nanargmin nancumprod nancumsum nanmax nanmean nanmedian nanmin
  nanpercentile nanprod nanquantile nanstd nansum nanvar ndim nonzero
  obj2sctype ones ones_like outer pad partition percentile piecewise
  place poly polyadd polyder polydiv polyfit polyint polymul polysub
  polyval printoptions prod product ptp put put_along_axis quantile
  ravel real real_if_close recfromcsv recfromtxt repeat require reshape
  resize roll rollaxis roots rot90 round round_ row_stack safe_eval save
  savetxt savez savez_compressed sctype2char searchsorted select
  set_printoptions set_string_function setbufsize setdiff1d seterr
  seterrcall setxor1d shape show_config sinc size sometrue sort
  sort_complex source split squeeze stack std sum swapaxes take
  take_along_axis tensordot tile trace transpose trapz tri tril
  tril_indices tril_indices_from trim_zeros triu triu_indices
  triu_indices_from typename union1d unique unwrap vander var vsplit
  vstack who zeros_like)


(define-functions numpy.fft
  fft fft2 fftfreq fftn fftshift hfft ifft ifft2 ifftn ifftshift ihfft
  irfft irfft2 irfftn rfft rfft2 rfftfreq rfftn)

(define-functions numpy.linalg
  cholesky cond det eig eigh eigvals eigvalsh inv lstsq matrix_power
  matrix_rank multi_dot norm pinv qr slogdet solve svd tensorinv
  tensorsolve)


;;;
;;; Simple Numpy Procedures
;;;

;; The following functions from `numpy` can't be imported automatically:

;;   bincount busday_count busday_offset can_cast concatenate copyto
;;   datetime_as_string dot empty_like inner is_busday lexsort
;;   may_share_memory min_scalar_type packbits putmask ravel_multi_index
;;   result_type shares_memory unpackbits unravel_index vdot where

;; The problem is that inspect.signature fails on the functions above.

(define (simple-numpy-proc name qualified-name
                           positional-parameters
                           keyword-parameters
                           #:object-type-str   [object-type-str   "fun"]
                           #:positional-excess [positional-excess #f]
                           #:keyword-excess    [keyword-excess    #f]
                           #:positional-types  [positional-types  #f]
                           #:keyword-types     [keyword-types     #f]
                           #:first-optional    [first-optional    #f]
                           #:result-type       [result-type       #f])
  (define object (obj object-type-str (get qualified-name)))
  (set! name (~a name))                                       ; allow symbol
  (set! positional-parameters (map ~a positional-parameters)) ; allow symbols
  (set! keyword-parameters    (map ~a keyword-parameters))    ; allow symbols
  (when positional-excess
    (set! positional-excess (~a positional-excess)))          ; allow symbol
  (when keyword-excess
    (set! keyword-excess (~a keyword-excess)))                ; allow symbol
  (unless positional-types
    (set! positional-types (make-list (length positional-parameters) #f)))
  (unless keyword-types
    (set! keyword-types    (make-list (length keyword-parameters)    #f)))  
  (pyproc object name qualified-name
          positional-parameters positional-types positional-excess 
          keyword-parameters    keyword-types    keyword-excess
          first-optional result-type))


(define (simple-numpy-builtin name qualified-name
                              positional-parameters
                              keyword-parameters
                              #:positional-excess [positional-excess #f]
                              #:keyword-excess    [keyword-excess    #f]
                              #:positional-types  [positional-types  #f]
                              #:keyword-types     [keyword-types     #f]
                              #:result-type       [result-type       #f])
  (pyproc->procedure
   (simple-numpy-proc name qualified-name
                      positional-parameters
                      keyword-parameters
                      #:object-type-str   "numpy.builtin"
                      #:positional-excess positional-excess
                      #:keyword-excess    keyword-excess
                      #:positional-types  positional-types
                      #:keyword-types     keyword-types
                      #:result-type       result-type)))


(define-delayed
  (define numpy.bincount
    (simple-numpy-builtin 'bincount     'numpy.bincount
                          '(x) '(weights minlength)))
  (define numpy.busday_count
    (simple-numpy-builtin 'busday_count 'numpy.busday_count
                          '(begindates enddates) '(weekmask holidays busdaycal out)))
  (define numpy.busday_offset
    (simple-numpy-builtin 'busday_offset 'numpy.busday_offset
                          '(dates offsets) '(roll weekmask holidays busdaycal out)))
  (define numpy.can_cast
    (simple-numpy-builtin 'can_cast 'numpy.can_cast
                          '(from_ to) '(casting)))
  (define numpy.concatenate
    (simple-numpy-builtin 'concatenate 'numpy.concatenate
                          '(as) '(axis out dtype casting)))
  (define numpy.copyto
    (simple-numpy-builtin 'copyto 'numpy.copyto
                          '(dst src) '(casting where)))
  (define numpy.datetime_as_string
    (simple-numpy-builtin 'datetime_as_string 'numpy.datetime_as_string
                          '(arr) '(unit timezone casting)))
  (define numpy.dot
    (simple-numpy-builtin 'dot 'numpy.dot
                          '(a b) '(out)))
  (define numpy.empty_like
    (simple-numpy-builtin 'empty_like 'numpy.empty_like
                          '(prototype) '(dtype order subok shape)))
  (define numpy.inner
    (simple-numpy-builtin 'inner 'numpy.inner
                          '(a b) '()))
  (define numpy.is_busday
    (simple-numpy-builtin 'is_busday 'numpy.is_busday
                          '(dates) '(weekmast holidays busycal out)))
  (define numpy.lexsort
    (simple-numpy-builtin 'lexsort 'numpy.lexsort
                          '(keys) '(axis)))
  (define numpy.may_share_memory
    (simple-numpy-builtin 'may_share_memory 'numpy.may_share_memory
                          '(a b) '(max_work)))
  (define numpy.min_scalar_type
    (simple-numpy-builtin 'min_scalar_type 'numpy.min_scalar_type
                          '(a) '()))
  (define numpy.packbits
    (simple-numpy-builtin 'packbits 'numpy.packbits
                          '(a) '(axis bitorder)))
  (define numpy.putmask
    (simple-numpy-builtin 'putmask 'numpy.putmask
                          '(a mask values) '()))
  (define numpy.ravel_multi_index
    (simple-numpy-builtin 'ravel_multi_index 'numpy.ravel_multi_index
                          '(multi_index dims) '(mode order)))
  (define numpy.result_type
    (simple-numpy-builtin 'result_type 'numpy.result_type
                          '() '()
                          #:positional-excess 'arrays_and_dtypes))
  (define numpy.shares_memory
    (simple-numpy-builtin 'shares_memory 'numpy.shares_memory
                          '(a b) '(max_work)))
  (define numpy.unpackbits
    (simple-numpy-builtin 'unpackbits 'numpy.unpackbits
                          '(a) '(axis count bitorder)))
  (define numpy.unravel_index
    (simple-numpy-builtin 'unravel_index 'numpy.unravel_index
                          '(indices shape) '(shape)))
  (define numpy.vdot
    (simple-numpy-builtin 'vdot 'numpy.vdot
                          '(a b) '()))
  (define numpy.where ; note: see numpy/ma/core.py for signature
    (simple-numpy-builtin 'where 'numpy.where
                          '(condition) '(x y))) 
  
  )
  
;;;
;;; Based on Universal Functions
;;;

;; The following universal functions won't work with `inspect.signature` but
;; since they all have similar signatures, we can import them automatically anyway.

;; Typical signatures of universal functions:

;;   numpy.absolute(x, /, out=None, *, where=True, casting='same_kind', order='K', dtype=None,
;;                        subok=True[, signature, extobj]) = <ufunc 'absolute'>
;;   numpy.add(x1, x2, /, out=None, *, where=True, casting='same_kind', order='K', dtype=None,
;;                        subok=True[, signature, extobj]) = <ufunc 'add'>
;;   numpy.arccos(x, /, out=None, *, where=True, casting='same_kind', order='K', dtype=None,
;;                        subok=True[, signature, extobj]) = <ufunc 'arccos'>

(define (get-ufunc-as-pyproc qualified-name)
  (define f (obj "ufunc" (get qualified-name)))
  (define nin  (getattr f "nin"))
  (define nout (getattr f "nout"))

  (define (names n)
    (match n
      [0 '()]
      [1 '("x")]
      [2 '("x1" "x2")]
      [3 '("x1" "x2" "x3")]
      [_  (for/list ([i n])
            (~a "x" (+ i 1)))]))

  (match (list nin nout)
    [(list n 1) ; e.g. absolute
     (define object                f)
     (define name                  (getattr object "__name__"))
     (define positional-parameters (names n))
     (define positional-types      (make-list (length positional-parameters) #f))
     (define positional-excess     #f)
     (define keyword-parameters    (list "out" "where" "same_kind" "order" "dtype" "subok"))
     (define keyword-types         (list #f    #f      #f          #f      #f      #f))
     (define keyword-excess        #f)
     (define first-optional        #f)
     (define result-type           #f)     
     (pyproc object name qualified-name
             positional-parameters positional-types positional-excess 
             keyword-parameters    keyword-types    keyword-excess
             first-optional result-type)]
    [_
     ; (displayln (list nin nout qualified-name))
     (error 'get-ufunc-as-pyproc "todo")]))

(define (get-ufunc qualified-name)
  ; (displayln (get-ufunc-as-pyproc qualified-name))
  (pyproc->procedure
   (get-ufunc-as-pyproc qualified-name)))

(define-syntax (define-numpy-universal-functions stx)
  (syntax-parse stx
    [(_ id ...)
     (define qualified-names (for/list ([id (syntax->list #'(id ...))])
                               (format-id id "numpy.~a" id)))
     (with-syntax ([(qualified-name ...) qualified-names])
       (syntax/loc stx
         (define-delayed
           (define qualified-name (get-ufunc 'qualified-name))
           ...)))]))

(define-numpy-universal-functions
  abs absolute add arccos arccosh arcsin arcsinh arctan arctan2
  arctanh bitwise_and bitwise_not bitwise_or bitwise_xor cbrt ceil conj
  conjugate copysign cos cosh deg2rad degrees divide  equal exp
  exp2 expm1 fabs float_power floor floor_divide fmax fmin fmod 
  gcd greater greater_equal heaviside hypot invert isfinite isinf isnan
  isnat lcm ldexp left_shift less less_equal log log10 log1p log2
  logaddexp logaddexp2 logical_and logical_not logical_or logical_xor
  matmul maximum minimum mod  multiply negative nextafter not_equal
  positive power rad2deg radians reciprocal remainder right_shift rint
  sign signbit sin sinh spacing sqrt square subtract tan tanh
  true_divide trunc
  ; Two outputs:
  #;divmod #;frexp #;modf) ; todo


;;;
;;; These 25 functions have no signature and are not universal.
;;; They require individual attention.

; todo

;; _add_newdoc_ufunc
;; _from_dlpack
;; add_docstring
;; add_newdoc_ufunc
;; arange
;; array
;; asanyarray
;; asarray
;; ascontiguousarray
;; asfortranarray
;; compare_chararrays
;; datetime_data
;; empty
;; fastCopyAndTranspose
;; frombuffer
;; fromfile
;; fromiter
;; frompyfunc
;; fromstring
;; geterrobj
;; nested_iters
;; promote_types
;; set_numeric_ops
;; seterrobj
;; zeros


;;;
;;; Random
;;;

;; The functions in `numpy.generator` are missing signatures.

(define (simple-numpy-random name qualified-name
                              positional-parameters
                              keyword-parameters
                              #:positional-excess [positional-excess #f]
                              #:keyword-excess    [keyword-excess    #f]
                              #:positional-types  [positional-types  #f]
                              #:keyword-types     [keyword-types     #f]
                              #:first-optional    [first-optional    #f]
                              #:result-type       [result-type       #f])
  (pyproc->procedure
   (simple-numpy-proc name qualified-name
                      positional-parameters
                      keyword-parameters
                      #:object-type-str   "numpy.random"
                      #:positional-excess positional-excess
                      #:keyword-excess    keyword-excess
                      #:positional-types  positional-types
                      #:keyword-types     keyword-types
                      #:first-optional    first-optional
                      #:result-type       result-type)))

; class numpy.random.Generator(bit_generator)

(define-delayed
  (define numpy.random.Generator
    (simple-numpy-random 'random.Generator 'numpy.random.Generator
                         '("bit_generator") '()))
  (define numpy.random.Generator.beta 
    (simple-numpy-random 'random.Generator.beta 'numpy.random.Generator.beta
                         '(a b) '(size)))
  (define numpy.random.beta 
    (simple-numpy-random 'random.beta 'numpy.random.beta
                         '(a b) '(size)))
  
  (define numpy.random.default_rng
    (simple-numpy-random 'random.default_rng 'numpy.random.default_rng
                         '(seed) '()
                         #:first-optional 0)) ; seed is the first optional
  #;(define numpy.random.choice
    (simple-numpy-random 'random.choice 'numpy.random.choice
                         '(a) '(size replace p)))
  )
