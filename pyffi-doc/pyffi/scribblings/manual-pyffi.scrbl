#lang scribble/manual
@;(require racket/gui/base)
@; The following command will build the manual and open it in a browser.
@; raco scribble +m --dest html --redirect-main http://docs.racket-lang.org manual-pyffi.scrbl && open html/manual-pyffi.html
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
   @hyperlink["https://github.com/soegaard/pyffi/"
              "github.com/soegaard/pyffi/"])


@title[#:tag "pyffi"]{pyffi - Use Python from Racket}

This library @tt{pyffi} allows you to use Python from Racket.

@author[@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]]

@;local-table-of-contents[]


@section[#:tag "introduction"]{Introduction}

The library @tt{pyffi} makes it possible to use Python libraries from Racket.

The bridge between Racket and Python is for now one-way only: a Racket
program can call Python function and invoke Python methods, but it's not possible (yet)
to pass Racket functions to the Python side.

Python libraries implemented in Python ought to work out of the box.

Python libraries implemented as C extensions might work - if the C
extension supports introspection via the Python module `inspect`. For
C extensions without introspection you can drop down to a low-level
FFI which works in the same style as the Racket C FFI.

Bindings for @tt{Numpy}, the popular package for numerical calculations, are provided.


@section[#:tag "release-information"]{Release Information}

This is the first release of the library, so be prepared to deal with
some rough edges here and there.

Report bugs to the @tt{pyffi} Github repository: @|pyffi-github|.

If you run into any questions, use the forum
@hyperlink["https://racket.discourse.group/"]{Racket Discourse}.

Alternatively, feel free to ask questions on the chat server 
@hyperlink["https://discord.com/invite/6Zq8sH5"]{Racket Discord}.


@section[#:tag "supported-platforms"]{Supported Platforms}

For now @tt{pyffi} supports Python 3.10 on macOS, Linux/Unix and Windows.


@section[#:tag "installation"]{Installation}

The plan is simple: install Python 3.10 then tell @tt{pyffi} where
Python is installed.

@subsection{Python 3.10}

In order to use @tt{pyffi} you need Python 3.10.

The official distribution of Python is here:
@hyperlink["https://www.python.org/downloads/"]{https://www.python.org/downloads/}

If you prefer to use an alternative source of distribution
(read your favorite package manager), that's fine too - as long as it
includes both the standard interpreter as well as the shared library @tt{libpython}.

@subsection{Configuration of @tt{pyffi}}

The last installation step consists of telling @tt{pyffi} where your
shared library @tt{libpython} is placed.

The easiest way of doing this is to run the script @tt{configure-pyffi}.

@itemlist[
          @item{Open a terminal.}
          @item{Check that @tt{python3} or @tt{python} invokes your Python interpreter.@linebreak[]
                If not, adjust your @tt{PATH}.}
          @item{Run: @tt{racket configure-pyff.rkt}}]

This will find the location of your shared library, print it, and,
store it in your Racket preferences under the key @tt{pyffi:libdir}.



@index-section[]


