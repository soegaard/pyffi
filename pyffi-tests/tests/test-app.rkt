#lang racket/base
(require pyffi/structs
         pyffi/python-c-api
         pyffi/python-initialization
         pyffi/python-environment
         pyffi/python-evaluation
         pyffi/python-types
         pyffi/python-builtins
         pyffi/python-operators
         pyffi/python-attributes
         racket/format
         racket/match)

(require (for-syntax racket/base syntax/parse))

(define (import-inspect)
  (define empty-from-list (PyList_New 0))
  (define mod:inspect (PyImport_ImportModuleEx "inspect" globals globals empty-from-list))
  (when mod:inspect
    (void (PyModule_AddObjectRef main "inspect" mod:inspect)))) ; 0=success

;;;
;;; Start Python and import "numpy"
;;;

(set-environment-variables)
(initialize)                 ; handles `main` and `builtins`
(import-inspect)
;                            ; import and initialize numpy before
;                            ; running the delayed initializers
(finish-initialization)      ; run delayed setters

(define-py inspect.signature (~fun ~py -> ~py))
(finish-initialization)

(inspect.signature (obj "pyfun" (get 'inspect.signature)))
(.parameters (inspect.signature (obj "pyfun" (get 'inspect.signature))))

