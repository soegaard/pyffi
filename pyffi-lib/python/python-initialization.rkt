#lang racket/base
(require "libpython.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-delayed.rkt"
         "python-constants.rkt"
         "structs.rkt")

(provide set-environment-variables
         initialize finish-initialization
         diagnostics)

;;;
;;; Configuration
;;;

(define program-full-path
  "/usr/local/Cellar/python@3.10/3.10.4/Frameworks/Python.framework/Versions/3.10/bin/python3.10")

(define home "/usr/local/Cellar/python@3.10/3.10.4/Frameworks/Python.framework/Versions/3.10")

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

;;;
;;; Setup Initial Environment
;;;
;;;
;;; Evaluation
;;;


(define (initialize)  
  (Py_Initialize)
  (initialize-main-and-builtins)
  (initialize-builtin-constants) ; uses `run`
  ; We can't run the initialization thunks here.
  ; The Python modules are loaded yet.
  ; (run-initialization-thunks)
  )

(define (finish-initialization)
  (run-initialization-thunks))
