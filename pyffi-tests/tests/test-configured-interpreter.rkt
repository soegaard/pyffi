#lang racket/base

(require pyffi
         rackunit
         racket/file
         racket/path)

(define configured-executable
  (get-preference 'pyffi:executable (λ () #f)))
(define configured-base-prefix
  (get-preference 'pyffi:home (λ () #f)))
(define configured-venv
  (and configured-executable
       (let* ([bin-dir (path-only (string->path configured-executable))]
              [root (simplify-path (build-path bin-dir 'up))])
         (and (file-exists? (build-path root "pyvenv.cfg"))
              root))))

(initialize)
(finish-initialization)

(define (run/string expression)
  (PyUnicode_AsUTF8 (obj-the-obj (run expression))))

;; The embedded interpreter must start like the executable selected by
;; `raco pyffi configure`. In particular, a venv executable must make Python
;; read pyvenv.cfg and apply its package-isolation and .pth-file semantics.
(when configured-executable
  (check-equal? (run/string "__import__('sys').executable")
                configured-executable)
  (check-equal? (run/string "__import__('sys').base_prefix")
                configured-base-prefix))
(when configured-venv
  (check-equal?
   (path->directory-path
    (simplify-path (string->path (run/string "__import__('sys').prefix"))))
   configured-venv))

;; CI creates these marker modules in the base interpreter, the venv and a
;; directory exposed only by a .pth file. Together they exercise the path
;; semantics that a manually appended site-packages directory cannot provide.
(when (getenv "PYFFI_TEST_VENV")
  (check-equal? (run/string "__import__('pyffi_precedence_marker').SOURCE")
                "venv")
  (check-equal?
   (run/string
    "'absent' if __import__('importlib.util', fromlist=['util']).find_spec('pyffi_base_only_marker') is None else 'present'")
   "absent")
  (check-equal? (run/string "__import__('pyffi_pth_marker').SOURCE") "pth"))
