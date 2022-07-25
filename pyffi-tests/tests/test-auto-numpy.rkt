#lang racket/base
(require pyffi pyffi/numpy
         racket/format racket/list racket/match
         (for-syntax racket/base racket/syntax syntax/parse))

;;;
;;; Start Python and import "numpy"
;;;

(set-environment-variables)
(initialize)                 ; handles `main` and `builtins`
(import-numpy)
;                            ; load the `numpy` module
;                            ; import and initialize numpy before
;                            ; running the delayed initializers
(finish-initialization)      ; run delayed setters



(define rng (numpy.random.default_rng 12345))
rng
(tell rng random -> ~py)
(tell rng beta 1 ~py 1 ~py -> ~py)
(numpy.random.beta 1 1)

;(get-fun-as-pyproc 'numpy.linalg.det)
;(define numpy.linalg.det (get-fun 'numpy.linalg.det))

'foo
;(get-fun-as-pyproc 'numpy.vander)
;(define vander (get-fun 'numpy.vander))

'bar
(define x (numpy.array '[1 2 3 5]))
(from (numpy.vander x))
(from (numpy.vander x #:increasing #t))
;(from (numpy.linalg.det (numpy.vander x)))



(from (numpy.stack (list (numpy.array '[1 2 3 4])
                         (numpy.array '[5 6 7 8]))))
      

(for/vector ([x (numpy.histogram '[0 1 1 2 2 2 3 4 4 4 4 4 5 5 6 6 6 7 7 7 7 7 7 8 8 9 10])])
  (from x))

(define det (get-fun 'numpy.linalg.det))
(from (det (numpy.array '[[1 2][3 4]])))

(displayln (repr (numpy.array '[[1 2][3 4]])))

(numpy.array '[[1 2][3 4]])

'---------------


(numpy.radians (numpy.array '[[0 90] [180 360]]))


(numpy.arange 5)
(numpy.arange 1 5)
(numpy.arange 2 5 1)
(numpy.arange 1001)



; (define numpy.absolute (get-ufunc 'numpy.absolute))

; (numpy.absolute (numpy.array '[[-1 2] [-3 3]]))

#;(struct pyproc (object name qualified-name
                positional-parameters positional-types positional-excess 
                keyword-parameters    keyword-types    keyword-excess
                result-type)
  #:transparent)


(newline)(newline)
(numpy.linalg.svd (numpy.array '[[1 2] [3 4]]))

;; (define n 100000)
;; (define as (make-vector n 1.))
;; (define bs (make-vector n 1.))
;; (time
;;  (for/sum ([a (in-vector as)]
;;            [b (in-vector bs)])
;;    (* a b)))

;; (define A (numpy.ones (vector n)))
;; (define B (numpy.ones (vector n)))
;; (time (numpy.dot A B))


