#lang racket/base
(require "../structs.rkt"
         "../python-c-api.rkt"
         "../python-initialization.rkt"
         "../python-environment.rkt"
         "../python-evaluation.rkt"
         "../python-types.rkt"
;         "numpy.rkt"
         racket/format
         )

(require (for-syntax racket/base
                     syntax/parse))

;;; Start Python and load "numpy"

(set-environment-variables)
(initialize)                 ; handles `main` and `builtins`
;(initialize-numpy)           ; load the `numpy` module
(finish-initialization)      ; run delayed setters

(id-bound? 'operator)
(void (run* "import operator as operator"))
(id-bound? 'operator)

(id-bound? 'builtins)

;(void (run* "import operator as operator"))
