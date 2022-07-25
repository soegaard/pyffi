#lang racket/base
(require "../python.rkt"
         "../numpy.rkt"
         "../numpy-core.rkt"
         ;; "../python-c-api.rkt"
         ;; "python-initialization.rkt"
         ;; "python-environment.rkt"
         ;; "python-evaluation.rkt"
         ;; "python-types.rkt"
         ;; ;"python-constants.rkt"
         ;; "python-builtins.rkt"
         ;; "python-operators.rkt"
         ;; "python-slice.rkt"
         racket/format
         racket/match)
(require (for-syntax racket/base syntax/parse))

;;;
;;; Start Python and import "numpy"
;;;

(set-environment-variables)
(initialize)                 ; handles `main` and `builtins`
(initialize-numpy)           ; load the `numpy` module
;                            ; import and initialize numpy before
;                            ; running the delayed initializers
(finish-initialization)      ; run delayed setters
(declare-special-prefix numpy)

;;;
;;; Tests
;;;


;;; Construction and Attributes

(numpy.zeros #(2 3))
(numpy.ndarray.tolist (numpy.zeros #(2 3)))
int16
(numpy.ndarray.tolist (numpy.array '((1 2 3) (4 5 6)) #:dtype int16))
(numpy.ndarray.tolist (numpy.array '((0 2 3) (4 5 6)) #:dtype double))
(numpy.empty #(0 1) #:dtype double #:order "C")
(define A (numpy.array '[[1 2 3 4]
                         [5 6 7 8]]))
; attributes
(equal? (.ndim A)  2)                             ; A.ndim
(equal? (.shape A) #(2 4))                        ; A.shape
(equal? (.shape A) #(2 4))
(equal? (.size A)  8)                              ; A.size
(equal? (obj-type-name (.dtype A)) "dtype[int64]") ; A.dtype
(equal? (.itemsize A) 8)                           ; A.itemsize
(equal? (obj-type-name (.data A)) "memoryview")    ; A.data


(define B (numpy.array '[[1.1 2.2 3.3]
                         [4.4 5.5 6.6]]))
(equal? (.ndim     B) 2)                             ; B.ndim
(equal? (.shape    B) #(2 3))                        ; B.shape
(equal? (.size     B) 6)                             ; B.size
(equal? (.itemsize B) 8)                             ; B.itemsize
(equal? (obj-type-name (.dtype B)) "dtype[float64]") ; B.dtype
(equal? (obj-type-name (.data  B))  "memoryview")    ; B.data


; The low-level constructor
#;(displayln (repr (numpy.ndarray ; #:shape #(2,2)
                                #:dtype float64 #:order #\F)))



;;; Helpers

(define (array A)  (numpy.array A))
(define (arange n) (numpy.arange n))


;; Numpy scalars behave as an `ndarray` so we need to use `item`
;; to convert the value from a numpy scalar to a Python scalar.
(define (.item    A [type ~py]) (tell A item                 -> type))
(define (.reshape A shape)      (tell A reshape  shape ~py   -> ~ndarray))

(define (from-array A)
  (define s (.shape A))
  (match s
    [(vector)     (A .item)]
    [(vector n)   (pylist->list (numpy.ndarray.tolist A))]
    [(vector m n) (for/list ([i m])
                    (pylist->list (numpy.ndarray.tolist (ref A i))))]
    [(vector m n o) (for/list ([i m])
                      (define Ai (ref A i))
                      (for/list ([j n])
                        (pylist->list (numpy.ndarray.tolist (ref Ai j)))))]    
    [_
     (displayln s)
     (error 'from-array "todo")]))

;;; Methods


(procedure-arity PyObject_CallMethodObjArgs)

'METHODS
'all
((numpy.all (array '[[1 2 3 4] [5 6 7 8]])) .item)
(equal? ((numpy.all (array '[[1 2 3 4] [5 6 7 8]])) .item)         #t)
(equal? ((numpy.all (array '[[1 2 3 4] [0 6 7 8]])) .item)          #f)
(equal? ((numpy.all (array '[[#t #f] [#t #t]]))     .item)          #f)
(equal? (from-array (numpy.all (array '[[#t #f] [#t #t]]) #:axis 0)) '(#t #f))
(equal? (from-array (numpy.all (array '[-1 4 5])))                   #t)
; todo [crash] (numpy.all (numpy.array '[[#t #t] [#f #t]] #:where (array '[[#t] [#f]])))
'any
(equal? ((numpy.any (array '[[1 2 3 4] [0 6 7 8]])) .item)      #t)
(equal? ((numpy.any (array '[[#t #f] [#t #t]]))     .item)      #t)
(equal? (from-array (numpy.any (array '[[#t #f] [#f #f]]) #:axis 0)) '(#t #f))
(equal? (from-array (numpy.any (array '[-1 0 5])))                   #t)
; todo [crash] (numpy.all (numpy.array '[[#t #t] [#f #t]] #:where (array '[[#t] [#f]])))
'arange
(equal? (from-array ((arange 6) .reshape #(2 3))) '((0 1 2) (3 4 5)))
'argmax
(let ([A (add ((arange 6) .reshape #(2 3)) 10)])
  (and (equal? (from-array A)                          '((10 11 12) (13 14 15)))
       (equal? (from-array (numpy.argmax A #:axis 0))  '(1 1 1))
       (equal? (from-array (numpy.argmax A #:axis 1))  '(2 2))))
'argmin
(let ([A (add ((arange 6) .reshape #(2 3)) 10)])
  (and (equal? (from-array A)                          '((10 11 12) (13 14 15)))
       (equal? (numpy.argmin A)                        0)
       (equal? (from-array (numpy.argmin A #:axis 0))  '(0 0 0))
       (equal? (from-array (numpy.argmin A #:axis 1))  '(0 0))))
'argpartition
(let ([A (array '[3 4 2 1])])
  (and (equal? (from-array (getitem A (numpy.argpartition A 3)))      '(2 1 3 4))
       (equal? (from-array (getitem A (numpy.argpartition A #(1 3)))) '(1 2 3 4))))
'argsort
(equal? (from-array (numpy.argsort (array '[3 1 2]))) '(1 2 0))
'astype
(equal? (from-array (numpy.ndarray.astype (array '[1. 2. 3.]) int32)) '(1 2 3))
'byteswap
(equal? (from-array (numpy.array '[1 256 8755] #:dtype int16)) '[1 256 8755])
(let ([A (numpy.array '[1 256 8755] #:dtype int16)])
  (equal? (from-array (numpy.ndarray.byteswap A #:inplace #t)) '(256 1 13090)))
'choose
(let ([choices '[[ 0  1  2  3] [10 11 12 13] [20 21 22 23] [30 31 32 33]]])
  (equal? (from-array (numpy.choose '[2 3 1 0] choices)) '(20 31 12 3)))
'clip
(equal? (from-array (numpy.clip (arange 10) 1 8)) '(1 1 2 3 4 5 6 7 8 8)) 
'compress
(equal? (from-array (numpy.compress '[0 1] (array '[[1 2] [3 4] [5 6]]) #:axis 0)) '((3 4)))
'-----
'diag
(equal? (from-array (numpy.diag (array '[[1 2] [3 4] [5 6]]))) '(1 4))
(equal? (from-array (numpy.diag ((arange 9) .reshape #(3 3)))) '(0 4 8))
(equal? (from-array (numpy.diag ((arange 9) .reshape #(3 3)) #:k  1)) '(1 5))
(equal? (from-array (numpy.diag ((arange 9) .reshape #(3 3)) #:k -1)) '(3 7))
(equal? (from-array (numpy.diag '[1 2])) '[[1 0] [0 2]])
'full
(equal? (from-array (numpy.full #(2 3) 4)) '((4 4 4) (4 4 4)))
(equal? (from-array (numpy.full #(2 3) 4.)) '((4. 4. 4.) (4. 4. 4.)))
(equal? (from-array (numpy.full #(2 3) +nan.0)) '((+nan.0 +nan.0 +nan.0) (+nan.0 +nan.0 +nan.0)))
'interp
(let ([xp (array '[1 2 3])] [fp (array '[3 2 0])])
  (equal? (from-array (numpy.interp 2.5 xp fp)) 1.0))
'isnan
(equal? (from-array (numpy.isnan +nan.0)) #t)
(equal? (from-array (numpy.isnan 42)) #f)



'----FFT----
#;'fft.fft
#;(equal? (from-array (numpy.fft.fft (arange 10)))
        '(45.0+0.0i        -5.0+15.3884177i -5.0+6.8819096i
          -5.0+3.63271264i -5.0+1.62459848i -5.0-1.33226763e-15i
          -5.0-1.62459848i -5.0-3.63271264i -5.0-6.8819096i
          -5.0-15.3884177i))


'----RANDOM----
(numpy.random.seed 42)
(let ([A (numpy.random.choice (array '[#f #t]) #:size 10)])
  ; count falst to true transitions:
  (numpy.count_nonzero (lt (ref A (slice 1 #f)) (ref A (slice #f -1)))))

(let ([prices (numpy.full 100 +nan.0)])
  (:= prices [(array '[0 25 60 -1])] (array '[80. 30. 75. 50.]))
  (define x        (arange (len prices)))
  (define is-valid (invert (numpy.isnan prices)))
  ; prices = np.interp(x=x, xp=x[is_valid], fp=prices[is_valid])
  (set! prices (numpy.interp x (getitem x is-valid) (getitem prices is-valid)))
  (set! prices (add prices (mul (numpy.random.randn (len prices)) 2.)))
  (from-array prices))
      

