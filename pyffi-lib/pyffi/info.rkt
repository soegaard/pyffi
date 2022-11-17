#lang info

(define deps       (list "base" "at-exp-lib"))
(define build-deps (list "base" "at-exp-lib"))

(define raco-commands
  (list (list "pyffi" 'pyffi/configure-pyffi "configure pyffi" #f)))
