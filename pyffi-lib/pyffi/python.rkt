#lang racket/base
(require "structs.rkt"
         "python-attributes.rkt"
         "python-builtins.rkt"
         "python-c-api.rkt"
         "python-define-delayed.rkt"
         "python-dict.rkt"
         "python-environment.rkt"
         "python-evaluation.rkt"
         "python-functions.rkt"
         "python-generator.rkt"
         "python-initialization.rkt"
         "python-import.rkt"
         "python-list.rkt"
         "python-module.rkt"
         "python-more-builtins.rkt"
         "python-operators.rkt"
         "python-slice.rkt"
         "python-string.rkt"
         "python-tuple.rkt"
         "python-types.rkt")

(provide (except-out
          (all-from-out
           "structs.rkt"
           "python-attributes.rkt"
           "python-builtins.rkt"
           "python-c-api.rkt"
           "python-define-delayed.rkt"
           "python-dict.rkt"
           "python-environment.rkt"
           "python-evaluation.rkt"
           "python-functions.rkt"
           "python-generator.rkt"
           "python-initialization.rkt"
           "python-import.rkt"
           "python-list.rkt"
           "python-module.rkt"
           "python-more-builtins.rkt"
           "python-operators.rkt"
           "python-slice.rkt"
           "python-string.rkt"
           "python-tuple.rkt"
           "python-types.rkt")
          ; the values are wrapped in an obj struct below
          builtins main
          
          ; The procedures run and run* return cpointers.
          ; Automatic `pr` conversion is provided below
          run run*))

(require "python-delayed.rkt")

;; Modules:  builtins, main

(define obj-builtins 'uninitialized-obj-builtins)
(define obj-main     'uninitialized-obj-main)

(add-initialization-thunk
 (λ ()
   (set! obj-builtins (obj "module" builtins))
   (set! obj-main     (obj "module" main))))

(provide (rename-out [obj-builtins builtins]
                     [obj-main     main]))

;; Automatic conversion:  run, run*

(require (for-syntax racket/base syntax/parse racket/syntax))
(require racket/format racket/string)

(define py-format-exception      'uninitialized-py-format-exception)
(define py-format-exception-only 'uninitialized-py-format-exception-only)
(define py-format-traceback      'uninitialized-py-format-traceback)

(add-initialization-thunk
 (λ ()
   (set! py-format-exception      (get 'traceback.format_exception))
   (set! py-format-exception-only (get 'traceback.format_exception_only))
   (set! py-format-traceback      (get 'traceback.format_tb))))


(define-syntax (handle-python-exception stx)
    ; When `run` and `run*`returns  NULL (represented as #f in Racket),
    ; it means an exception occurred on the Python side.
    ; We must fetch and normalize the exception, exception value and the traceback.
    ; Fetching the exception clears the exception flags on the Python side.
    ; Formatting the exception as a string is done useing the Python module `traceback`.
    (syntax-parse stx
      [(_handle-python-exception qualified-name result:id)
       (syntax/loc stx
         (cond
           ;  everything went fine
           [result result]
           ;  an exception was raised
           [else
            ; fetch exception (and clear flags), then normalize the exception value
            (define-values (ptype pvalue ptraceback) (PyErr_Fetch))
            (define-values (ntype nvalue ntraceback) (PyErr_NormalizeException ptype pvalue ptraceback))
            ; format the exception so we can print it
            #;(define msg (let ([args (flat-vector->py-tuple (vector ntype nvalue ntraceback))])
                            (python->racket (PyObject_Call py-format-exception args #f))))
            (define msg (let ([args (flat-vector->py-tuple (vector ntype nvalue))])
                          (define x (PyObject_Call py-format-exception-only args #f))
                          (py-list->list/pr x)))
            (define tb  (and ntraceback
                             (let ([args (flat-vector->py-tuple (vector ntraceback))])
                               (py-list->list/pr (PyObject_Call py-format-traceback args #f)))))
            ;; Racket error message convention:
            ;;   ‹srcloc›: ‹name›: ‹message›;
            ;;     ‹continued-message› ...
            ;;     ‹field›: ‹detail›
            (define full-msg
              (~a ; "src-loc" ": "
               qualified-name ": " "Python exception occurred" ";\n"
               (string-append* (map (λ (m) (~a " " (pystring->string m))) msg)) 
               (if tb (string-append* (map (λ (m) (~a " " m)) tb)) "")))
            (raise (exn full-msg (current-continuation-marks)))]))]))

(define (prrun x)
  (define result (run x))
  (handle-python-exception 'run result)
  (pr result))

(define (prrun* x)
  (define result (run* x))  
  (handle-python-exception 'run* result)
  (void result))

(provide (rename-out [prrun  run]
                     [prrun* run*]))
