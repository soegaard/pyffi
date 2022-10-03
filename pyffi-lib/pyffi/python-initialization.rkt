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
(define program-full-path "python3.10")

(define home (get-preference 'pyffi:data (λ () #f)))
(unless home
  (parameterize ([current-output-port (current-error-port)])
    (displayln "There is no preference for 'pyffi:data' set.")
    (displayln "You must set the 'pyffi:data' preference to the home folder of python.")
    (displayln "The most convenient way to do this, is to run `raco pyffi configure`.")
    (displayln "See details in the documentation.")
    (exit 1)))


#;(define program-full-path
   "/usr/local/Cellar/python@3.10/3.10.4/Frameworks/Python.framework/Versions/3.10/bin/python3.10")

; (define home "/usr/local/Cellar/python@3.10/3.10.4/Frameworks/Python.framework/Versions/3.10")


(define libdir (get-preference 'pyffi:libdir (λ () #f)))
(unless libdir
  (parameterize ([current-output-port (current-error-port)])
    (displayln "There is no preference for 'pyffi:libdir' set.")
    (displayln "You must set the 'pyffi:libdir' preference to the home folder of python.")
    (displayln "The most convenient way to do this, is to run `raco pyffi configure`.")
    (displayln "See details in the documentation.")
    (exit 1)))

(define (set-environment-variables)
  (define (decode s) (Py_DecodeLocale s #f))
  (Py_SetProgramName (decode "python3.10"))
  ; (Py_SetProgramName (decode (build-path libdir)))
  ; (Py_SetPath (Py_DecodeLocale (get-preference 'pyffi:data (λ () #f)) #f))
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


#;(define (initialize)
  (set-environment-variables)
  (Py_Initialize)
  (initialize-main-and-builtins)
  (initialize-builtin-constants) ; uses `run`
  ; We can't run the initialization thunks here.
  ; The Python modules are loaded yet.
  #;(run-initialization-thunks))


(require ffi/unsafe
         #;(only-in ffi/unsafe malloc cast _cpointer ptr-ref cpointer-tag cpointer-push-tag!))


(define (initialize)
  ; (set-environment-variables)
  ; (displayln PyConfig-tag) ; 'PyConfig
  ; (define config (cast (ptr-add (malloc _PyConfig) 0) _pointer _PyConfig-pointer))

  ;; Pre Initialization

  (define preconfig (cast (malloc (ctype-sizeof _PyPreConfig))
                            _pointer _PyPreConfig*))

  ; (define preconfig (make-PyPreConfig 0 0 0 0 0 0 0 0 0 0))
  
  #;(displayln "Before PyPreConfig_InitPythonConfig")
  (PyPreConfig_InitPythonConfig preconfig)
  #;(displayln "PyPreConfig_InitPythonConfig\n")


  (set-PyPreConfig-utf8_mode! preconfig 1)
  (define (decode s) (Py_DecodeLocale s #f))
  ; (define (decode s) s)

  
  #;(displayln "Before Py_PreInitialize")
  (let ([status (Py_PreInitialize preconfig)])
    (unless (zero? (PyStatus_Exception status))
      (Py_ExitStatusException status)))
  #;(displayln "After Py_PreInitialize\n")

  
  ;; Initialization

  (define config (cast (malloc (ctype-sizeof _PyConfig))
                       _pointer _PyConfig-pointer))
  
  #;(displayln "Before InitPythonConfig")
  (PyConfig_InitPythonConfig config)
  #;(displayln "After InitPythonConfig\n")


  (set-PyConfig-home!         config (decode home))
  ; (set-PyConfig-program_name! config (decode "python3.10"))
  (set-PyConfig-platlibdir! config   (decode (string-append home "/" "lib/python3.10/site-packages")))
  (let ([pythonpath (getenv "PYTHONPATH")])
    (when pythonpath
      (set-PyConfig-pythonpath_env! config (decode pythonpath))))
  
  #;(displayln "Before InitializeFromConfig")
  (let ([status (Py_InitializeFromConfig config)])
    #;(displayln "Before exception check")
    (unless (zero? (PyStatus_Exception status))
      (Py_ExitStatusException status))
    #;(displayln "After InitializeFromConfig"))

  (initialize-main-and-builtins)
  (initialize-builtin-constants) ; uses `run`

  ; We can't run the initialization thunks here.
  ; The Python modules are loaded yet.
  #;(run-initialization-thunks))


(define (post-initialize)
  (run-initialization-thunks))

(define (finish-initialization)
  (run-initialization-thunks))
