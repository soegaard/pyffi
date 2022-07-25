#lang scribble/manual
@;(require racket/gui/base)
@; The following command will build the manual and open it in a browser.
@; raco scribble +m --dest html --redirect-main http://docs.racket-lang.org manual-sketching.scrbl && open html/manual-sketching.html
@(require racket/sandbox racket/format racket/file racket/runtime-path racket/string racket/list) 
@(require scribble/core scribble/html-properties (only-in xml cdata))
@(require scribble/example)
@(require (for-syntax racket/base syntax/parse))

@; Used to reference other manuals.
@(define reference.scrbl '(lib "scribblings/reference/reference.scrbl"))

@; Long urls
@(define (wikipedia name . preflow)
   (define url (string-append "https://en.wikipedia.org/wiki/" name))
   @margin-note{@hyperlink[url (list* @bold{Wikipedia: } " " preflow)]})

@(define (wikipedia/section url . preflow)
   @margin-note{@hyperlink[url (list* @bold{Wikipedia: } " " preflow)]})

@(define license-link
   @hyperlink["https://creativecommons.org/licenses/by-nc-sa/4.0/"
              "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License."])

@(define pyffi-github
   @hyperlink["https://github.com/soegaard/pyffi/" "https://github.com/soegaard/pyffi/"])


@title[#:tag "pyffi"]{pyffi - Use Python from Racket}

This module @tt{pyffi} allows you to use Python from Racket.

@author[@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]]

@;local-table-of-contents[]

@section[#:tag "introduction"]{Introduction}





@;-------------------
@;-------------------
@;-------------------
           

@index-section[]


