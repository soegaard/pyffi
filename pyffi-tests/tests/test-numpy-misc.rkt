#lang racket/base
(require "../python.rkt" "../numpy.rkt"
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
