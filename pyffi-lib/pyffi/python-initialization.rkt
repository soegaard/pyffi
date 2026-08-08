#lang racket/base
(require "libpython.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-delayed.rkt"
         "python-constants.rkt"
         "structs.rkt"
         racket/file
         racket/path
         racket/string)

(provide set-environment-variables
         initialize 
         post-initialize finish-initialization
         diagnostics)

;;;
;;; Configuration
;;;

(define fallback-program-name "python3")

;; PYTHONHOME resolution order:
;;   1. 'pyffi:home preference (explicit user config via raco pyffi configure)
;;   2. 'pyffi:data preference (legacy fallback for old configurations)
;;   3. The natipkg companion's package root, if a natipkg is installed
;;      and bundles a relocatable Python (libpython + stdlib).
;;   4. Error — pyffi is not configured.
(define home
  (or (get-preference 'pyffi:home (λ () #f))
      (get-preference 'pyffi:data (λ () #f))
      (let ([root (pyffi-natipkg-root)])
        ;; `simple-form-path` collapses any `..` segments introduced
        ;; by `pkg-directory`'s symlinked-install layout, so
        ;; Py_Initialize gets a canonical PYTHONHOME.
        (and root (path->string (simple-form-path root))))))
(unless home
  (raise (exn:fail:pyffi:not-configured
          (string-join
           '("pyffi is not configured: neither 'pyffi:home nor 'pyffi:data is set,"
             "and no natipkg companion (e.g. pyffi-aarch64-linux-natipkg) is installed."
             "Either install a natipkg companion to use a bundled Python, or run"
             "`raco pyffi configure /path/to/python3` to point at a system install.")
          "\n")
          (current-continuation-marks))))

(define executable (get-preference 'pyffi:executable (λ () #f)))
(when (and (not executable)
           (get-preference 'pyffi:venv (λ () #f)))
  (raise (exn:fail:pyffi:not-configured
          (string-join
           '("pyffi's virtual-environment configuration predates executable tracking."
             "Rerun `raco pyffi configure /path/to/python3` before using pyffi.")
           "\n")
          (current-continuation-marks))))


;; The actual libpython load is handled by libpython.rkt, which knows
;; how to discover the library across env vars, user prefs, the
;; natipkg companion and the dynamic loader.

(define (set-environment-variables)
  (define (decode s) (Py_DecodeLocale s #f))
  ;; These setters are deprecated, but remain available across every Python
  ;; version pyffi supports. They also avoid depending on PyConfig's changing
  ;; C structure layout while preserving ordinary Python startup semantics.
  (cond
    [executable
     ;; Leave PYTHONHOME unset so Python can find pyvenv.cfg beside the
     ;; configured executable and apply normal virtual-environment semantics.
     (Py_SetProgramName (decode executable))]
    [else
     ;; A bundled natipkg has no external executable to discover from.
     (Py_SetProgramName (decode fallback-program-name))
     (Py_SetPythonHome (decode home))]))

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
