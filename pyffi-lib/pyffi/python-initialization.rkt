#lang racket/base
(require "libpython.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-delayed.rkt"
         "python-constants.rkt"
         "structs.rkt"
         racket/file)

(provide set-environment-variables
         initialize 
         post-initialize finish-initialization
         diagnostics)

;;;
;;; Configuration
;;;

; (define program-full-path "/Library/Frameworks/Python.framework/Versions/3.10/bin/python3.10")
(define program-full-path "pyffi")

(define home (get-preference 'pyffi:data (λ () #f)))
(unless home
  (parameterize ([current-output-port (current-error-port)])
    (displayln "There is no preference for 'pyffi:data' set.")
    (displayln "You must set the 'pyffi:libdir' preference to the home folder of python.")
    (displayln "The most convenient way to do this, is to run `raco pyffi configure`.")
    (displayln "See details in the documentation.")
    (exit 1)))


#;(define program-full-path
   "/usr/local/Cellar/python@3.10/3.10.4/Frameworks/Python.framework/Versions/3.10/bin/python3.10")

; (define home "/usr/local/Cellar/python@3.10/3.10.4/Frameworks/Python.framework/Versions/3.10")

(define (set-environment-variables)
  (define (decode s) (Py_DecodeLocale s #f))
  (Py_SetProgramName (decode program-full-path))
  (Py_SetPythonHome  (decode home)))

;;;
;;; Diagnostics
;;;


(define (diagnostics)
  (define (encode s) (and s (Py_EncodeLocale s #f))) ; wchar -> string
  (displayln (list 'ProgramName     (encode (Py_GetProgramName))))
  (displayln (list 'Prefix          (encode (Py_GetPrefix))))
  (displayln (list 'ExecPrefix      (encode (Py_GetExecPrefix))))
  (displayln (list 'ProgramFullPath (encode (Py_GetProgramFullPath))))
  (displayln (list 'Path            (encode (Py_GetPath))))
  (displayln (list 'PythonHome      (encode (Py_GetPythonHome)))))

(diagnostics)
;;;
;;; Setup Initial Environment
;;;
;;;
;;; Evaluation
;;;


(define (initialize)
  (set-environment-variables)
  (Py_Initialize)
  (initialize-main-and-builtins)
  (initialize-builtin-constants) ; uses `run`
  ; We can't run the initialization thunks here.
  ; The Python modules are loaded yet.
  #;(run-initialization-thunks))

(define (post-initialize)
  (run-initialization-thunks))

(define (finish-initialization)
  (run-initialization-thunks))
