#lang at-exp racket/base

;;;  This module loads the shared library `libpython` and
;;;  provides the form `define-python` which is used to 
;;;  create bindings for Python's C-API.

(provide define-python)

;;; Imports

(require ffi/unsafe ffi/unsafe/define racket/file) 


;;; Configuration

(define libpython-folder (get-preference 'pyffi:libdir (λ () #f)))
#;(unless libpython-folder
  (parameterize ([current-output-port (current-error-port)])
    (displayln "There is no preference for 'pyffi:libdir' set.")
    (displayln "In order for `pyffi` to find the shared library `libpython3` (or `libpython3.10`) ")
    (displayln "you must set the 'pyffi:libdir' preference to the folder of the shared library.")
    (displayln "The most convenient way to do this, is to run `raco pyffi configure`.")
    (displayln "See details in the documentation.")
    (exit 1)))


(define extension
  (case (system-type 'os)
    [(macosx)  "dylib"]
    [(unix)    "so"]
    [(windows) "dll"]
    [else      (error 'internal-error:extension "File a bug report on Github.")]))

(define (build-full-path name)
  (if libpython-folder
      (build-path libpython-folder
                  (string->path (string-append name "." extension)))
      (string->path (string-append name "." extension))))

(define libpython-path
  (or (for/first ([name '("libpython3.10" "libpython310" "libpython3")]
                  #:when (file-exists? (build-full-path name)))
        (build-full-path name))
      ;; Github Action (Ubuntu)
      (build-full-path "libpython3.10")))



; (displayln (list 'libpython-path libpython-path))

; Note: If the Python interpreter loads a shared library dynamically,
;       it needs access to the Python C-API. To make the symbols
;       exported by a shared library visible to other shared libaries,
;       we need to use a "flat namespace" and therefore use `#:global? #t`,
;       when loading the library.

(define-ffi-definer define-python (ffi-lib libpython-path #:global? #t))
