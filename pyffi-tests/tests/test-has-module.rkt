#lang racket/base
(require pyffi/structs
         pyffi/python-c-api
         pyffi/python-initialization
         pyffi/python-environment
         pyffi/python-evaluation
         pyffi/python-types
         racket/format
         )

(require (for-syntax racket/base
                     syntax/parse))

;;; Start Python and load "numpy"

(initialize)                 ; handles `main` and `builtins`
;(initialize-numpy)           ; load the `numpy` module
(finish-initialization)      ; run delayed setters

(id-bound? 'operator)
(void (run* "import operator as operator"))
(id-bound? 'operator)

(id-bound? 'builtins)

;(void (run* "import operator as operator"))
