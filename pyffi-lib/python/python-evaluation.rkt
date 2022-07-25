#lang racket/base
(provide run
         run*)

(require "structs.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         racket/format)

(require (for-syntax racket/base syntax/parse racket/format))


; These values represent the start symbol from the Python grammar.
(define Py_single_input 256)  ; single statement,        used for interactive interpreter loop 
(define Py_file_input   257)  ; sequences of statements, used for files
(define Py_eval_input   258)  ; isolated expression,     used with Py_CompileString


(define (run code [result-type #f])
  ; run: evaluates a single expression and returns the value
  (define result (PyRun_String (~a code "\n") Py_eval_input globals globals))
  (if result-type
      (let ([to (pytype-python-to-racket result-type)])
        (if to
            (to result)
            result))
      result))

; This version doesn't allow to set the source location
#;(define (run* code)
    ; run*: evalutes a sequence of statements
    ; (write   (~a code "\n\n")) (newline) (display (~a code "\n")) (newline)
    (PyRun_String (~a code "\n") Py_file_input   globals globals))

#;(define (run* code [source-location-string "<racket-string>"])
  ; run*: evalutes a sequence of statements
  ; (write   (~a code "\n\n")) (newline) (display (~a code "\n")) (newline)
  (define code-object (Py_CompileString (~a code "\n") source-location-string Py_file_input))
  (PyEval_EvalCode code-object globals globals))


(define-syntax (run* stx)
  (syntax-parse stx
    [(_run* code)
     (define source (syntax-source stx))
     (define line   (syntax-line stx))
     (define col    (syntax-column stx))
     (define location (~a "<" (or source "") ":" (or line "") ":" (or col "") ">"))
     (with-syntax ([location location])
       (syntax/loc stx
         (let ([code-object (Py_CompileString (~a code "\n") location Py_file_input)])
           (when code-object
             (PyEval_EvalCode code-object globals globals)))))]))
