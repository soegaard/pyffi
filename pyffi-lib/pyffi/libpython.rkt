#lang at-exp racket/base

;;;  This module loads the shared library `libpython` and
;;;  provides the form `define-python` which is used to 
;;;  create bindings for Python's C-API.

(provide define-python)

;;; Imports

(require ffi/unsafe ffi/unsafe/define racket/file racket/list) 


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


;; find-libpython3 : (or/c path-string? #f) -> (or/c path? #f)
;;   If libpython-folder is a directory, return the path to a file named
;;     libpython3.xx.<extension>
;;   inside that directory, or #f if none is found or folder is #f.
(define (find-libpython3 libpython-folder)
  (cond
    [(not libpython-folder) #f]
    [else
     (define dir (path->complete-path libpython-folder))
     (define rx
       ;; Example on macOS: ^libpython3\.[0-9][0-9]\.dylib$
       (regexp (format "^libpython3\\.[0-9][0-9]~a$"
                       (regexp-quote
                        (string-append "." extension)))))
     (with-handlers ([exn:fail? (λ (_e) #f)])
       (for/or ([p (in-list (directory-list dir))])
         (let-values ([(base name must-dir?) (split-path p)])
           (and name
                (let ([s (path->string name)])
                  (and (regexp-match? rx s)
                       (build-path libpython-folder p)))))))]))


(define (build-full-path name)
  (if libpython-folder
      (build-path libpython-folder
                  (string->path (string-append name "." extension)))
      (string->path (string-append name "." extension))))

(define libpython-path
  (or (find-libpython3 libpython-folder) ; An absolute, full path
      (for/first ([name '("libpython3" "libpython3.10" "libpython310"
                                       )]
                  #:when (file-exists? (build-full-path name)))
        (build-full-path name))
      ;; Github Action (Ubuntu)
      ;;   On Github Action the `raco pyffi configure` is run after
      ;;   the documentation is rendered, so we need to provide the version here.
      "libpython3.14"))  


; Note: If the Python interpreter loads a shared library dynamically,
;       it needs access to the Python C-API. To make the symbols
;       exported by a shared library visible to other shared libaries,
;       we need to use a "flat namespace" and therefore use `#:global? #t`,
;       when loading the library.

(require pyffi/parameters)
(define lib (ffi-lib libpython-path #:global? #t))

(define-ffi-definer define-python lib #:default-make-fail make-not-available)

