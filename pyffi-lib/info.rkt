#lang info

(define collection 'multi)

(define deps '("base"
               ;; If we want to distribute Python as a package, we will need:
               ;; ("python-i386-macosx"          #:platform "i386-macosx")
               ;; ("python-x86_64-macosx"        #:platform "x86_64-macosx")
               ;; ("python-ppc-macosx"           #:platform "ppc-macosx")
               ;; ("python-aarch64-macosx"       #:platform "aarch64-macosx")
               ;; ("python-win32-i386"           #:platform "win32\\i386")
               ;; ("python-win32-x86_64"         #:platform "win32\\x86_64")
               ;; ("python-win32-arm64"          #:platform "win32\\arm64")
               ;; ("python-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")
               ))

(define build-deps '())

(define pkg-desc "Use Python from Racket - Implementation part without documentation")

(define pkg-authors '(soegaard))

(define version "1.0")

(define test-responsibles '((all jensaxel@soegaard.net)))
