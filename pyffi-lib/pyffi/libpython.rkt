#lang at-exp racket/base

;;;  This module loads the shared libarary `libpython` and
;;;  provides the form `define-python` which is used to 
;;;  create bindings for Python's C-API.

(provide define-python)

;;; Imports

(require ffi/unsafe ffi/unsafe/define)


;;; Configuration

;; (define folder  "/Users/soegaard/Dropbox/GitHub/build-python/cpython")
;; (define libname "libpython3.12") ; .dylib
;; (define path    (build-path folder libname))


(define brew-folder  "/usr/local/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/lib")            ; stdlib/python3.10 
(define brew-folder2 "/usr/local/Cellar/python@3.10/3.10.4/Frameworks/Python.framework/Versions/3.10/lib")
(define brew-libname "libpython3.10.dylib")

; 
; /Library/Frameworks/Python.framework/Versions/3.10/lib/libpython3.10.dylib

(define brew-path   (build-path brew-folder2 brew-libname))

; Note: If the Python interpreter loads a shared library dynamically,
;       it needs access to the Python C-API. To make the symbols
;       exported by a shared library visible to other shared libaries,
;       we need to use a "flat namespace" and therefore use `#:global? #t`,
;       when loading the library.

(define-ffi-definer define-python (ffi-lib brew-path #:global? #t))



;; soegaard@mbp2 tmp % ./python3 -m sysconfig
;; Platform: "macosx-12-x86_64"
;; Python version: "3.10"
;; Current installation scheme: "osx_framework_library"

;; Paths: 
;; data =        "/usr/local"
;; scripts =     "/usr/local/bin"

;; platlib =     "/usr/local/lib/python3.10/site-packages"
;; purelib =     "/usr/local/lib/python3.10/site-packages"

;; include =     "/usr/local/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/include/python3.10"
;; platinclude = "/usr/local/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/include/python3.10"

;; platstdlib =  "/usr/local/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/lib/python3.10"
;; stdlib =      "/usr/local/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/lib/python3.10"
