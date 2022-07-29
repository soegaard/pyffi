#lang scribble/manual
@;(require racket/gui/base)
@; The following command will build the manual and open it in a browser.
@; raco84 scribble +m --dest html --redirect-main http://docs.racket-lang.org manual-pyffi.scrbl && open html/manual-pyffi.html
@(require racket/sandbox racket/format racket/file racket/runtime-path racket/string racket/list) 
@(require scribble/core scribble/html-properties (only-in xml cdata))
@(require scribble/example)
@(require (for-syntax racket/base syntax/parse))
@(require (for-label pyffi))

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

@(define (python-docs query text)
   @hyperlink[(~a "https://docs.python.org/3/search.html?q=" query )]{@|text|})


@(require scribble/example)

@(require racket/sandbox
          scribble/example)

@(define factory (make-base-eval-factory (list)))
@(define (make-pyffi-eval)
   (let ([e (factory)])
     (e '(require pyffi))
     e))
@(define pe (make-pyffi-eval))



@title[#:tag "pyffi"]{pyffi - Use Python from Racket}

@defmodule[pyffi]

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

Optional: install @hyperlink["https://numpy.org/"]{NumPy} if you want
to use @racket[pyffi/numpy].


@subsection{Python 3.10}

In order to use @tt{pyffi} you need Python 3.10.

The official distribution of Python is here:
@hyperlink["https://www.python.org/downloads/"]{https://www.python.org/downloads/}

If you prefer to use an alternative source of distribution
(read your favorite package manager), that's fine too - as long as it
includes both the standard interpreter as well as the shared library @tt{libpython}.

@subsection{One-time Configuration}

The last installation step consists of telling @tt{pyffi} where your
shared library @tt{libpython} is placed.

The easiest way of doing this is to run the script @tt{configure-pyffi}.

@itemlist[
          @item{Open a terminal.}
          @item{Check that @tt{python3} or @tt{python} invokes your Python interpreter.@linebreak[]}
          @item{Run: @tt{raco pyffi configure}}]

If the command @tt{python3} is not in your path, then you can write this instead:

@itemlist[
          @item{@tt{raco pyffi configure <path-to-your-python-command>}}]

This will find the location of your shared library, print it, and,
store it in your Racket preferences under the key @tt{pyffi:libdir}.

You are now ready to use @tt{pyffi}.


@subsection{Optional: @tt{NumPy} for scientific computing}

The Python package NumPy has the slogan:

@nested[#:style 'inset]{The fundamental package for scientific computing with Python.}

If you want to use @tt{pyffi/numpy} then you need to install @tt{NumPy}
in your Python environment first.

There are many ways of installing @tt{NumPy}, but the simplest is to use
the following in a terminal:

@itemlist[
          @item{@tt{python3 -m pip install numpy}}]



@section{An introduction to @tt{pyffi}}

The @tt{pyffi} library makes it possible to use Python libraries from a Racket program.

A Racket program can start a Python process by requiring @racket[pyffi] and calling
@racket[initialize]. After the initialization @racket[run] and @racket[run*] can
be used to evaluate expressions and statements in the Python process.

@examples[#:label #f #:eval pe
          (require pyffi)
          (initialize)
          (post-initialize)
          (run "1+2")]

Here Racket starts an embed Python process.
The Python "1+2" is parsed, compiled and evaluated by Python.
The resulting Python value 3 is then converted examples into the Racket value 3.

@subsection{Atomic values: numbers, booleans and @tt{None}}

Atomic Python values (numbers, booleans and None) are automatically converted
to their corresponding Racket values.

@examples[#:label #f #:eval pe
          (require pyffi)
          (initialize)
          (post-initialize)
          (run "12")
          (run "34.")
          (run "5+6j")
          (run "False")
          (run "True")
          (list (run "None"))]

@subsection{Compound Values: strings, tuples, lists, and, dictionaries}

Compound (non-atomic) Python values such as strings, tuples, lists and dicts are not converted
to Racket values. Instead they are wrapped in a struct named #racket[obj].
Due to a custom printer handler these wrapped values print nicely.

@examples[#:label #f #:eval pe
          (run "'Hello World'")
          (run "(1,2,3)")
          (run "[1,2,3]")
          (run "{'a': 1, 'b': 2}")]

The values display nicely too:

@examples[#:label #f #:eval pe
          (displayln (run "'Hello World'"))
          (displayln (run "(1,2,3)"))
          (displayln (run "[1,2,3]"))
          (displayln (run "{'a': 1, 'b': 2}"))]


Printing and displaying a Python object use the
@python-docs["object.__repr__"]{__repr__} and @python-docs["object.__str__"]{__str__}
methods of the object respectively.

The idea is that Racket gains four new data types: @tt{pystring}, @tt{pytuple}, 
@tt{pylist} and @tt{pydict}. @margin-note{Conversion between hash tables and pydicts
needs to be implemented, before it can be used in the introduction}
                                                   
To convert a compound value use @racket[pystring->string], @racket[pytuple->vector]
or @racket[pylist->list].

@examples[#:label #f #:eval pe
          (pystring->string (run "'Hello World'"))
          (pytuple->vector (run "(1,2,3)"))
          (pylist->list (run "[1,2,3]"))]

Similarly, you can convert Racket values to Python ones.

@examples[#:label #f #:eval pe
          (string->pystring "Hello World")
          (vector->pytuple #(1 2 3))
          (list->pylist '(1 2 3))]

It's important to note that creating Python values using
@racket[string->pystring], @racket[vector->pytuple] and @racket[list->pylist]
is much more efficient that using @racket[run]. The overhead of @racket[run]
is due to the parsing and compiling of its its input string. In contrast
@racket[string->pystring] and friends use the C API to create the
Python values directly.

The data types have also have constructors:

@examples[#:label #f #:eval pe
          (pystring #\H #\e #\l #\l #\o)
          (pytuple 1 2 3)
          (pylist 1 2 3)]

The new types @tt{pystring}, @tt{pytuple} and @tt{pylist} can be used with @racket[for].

@examples[#:label #f #:eval pe
          (for/list ([x (in-pystring (string->pystring "Hello"))]) x)
          (for/list ([x (in-pytuple (vector->pytuple #(1 2 3)))]) x)
          (for/list ([x (in-pylist (list->pylist '(1 2 3)))]) x)]


@subsection{Builtin functions and modules}

@margin-note{Use @racket[run] for expressions and @racket[run*] for statements.}
The previous sections showed how to evaluate expressions using the Python interpreter.
Now we will look at statements.

@examples[#:label #f #:eval pe
          (run* "x = 1+2")]

Here the statement @tt{x = 1+2} is parsed, compiled and executed.
The result of the expression @tt{1+2} is stored in the global variable @tt{x}.

To retrieve the value of the Python variable @tt{x} we could use @racket[run]:
@examples[#:label #f #:eval pe
          (run "x")]

But due to the overhead of @racket[run] it is better to make a direct
variable reference.

@examples[#:label #f #:eval pe
          main.x]

Here @tt{main} is the name we have given to the module used for the 
global namespace of the Python interpreter. The dotted identifier @tt{main.x}
thus references the variable @tt{x} in the global namespace.

@margin-note{The import is done with @linebreak[] @tt{import builtins}}
In a standard Python interpreter nothing is imported when the interpreter
is started, but with @racket[pyffi] the module @python-docs["builtins"]{builtins}
is imported at startup. 

Since Python modules are first class values, we can see their printed 
representations:

@examples[#:label #f #:eval pe
          main
          builtins]

@margin-note{Table of @hyperlink["https://docs.python.org/3/library/functions.html"]{Built-in functions}}
The module @tt{builtins} is also how we access the built-in Python functions.

@examples[#:label #f #:eval pe
          (builtins.abs -7)
          (builtins.list "Hello")
          (builtins.range 2 5)
          (builtins.list (builtins.range 2 5))]

If you find the name @tt{builtins} too long, then you can give it a new, shorter name.

@examples[#:label #f #:eval pe
          (define b builtins)
          (b.abs -7)]

If you access the @tt{abs} functions directly, you get a callable object:

@examples[#:label #f #:eval pe
          b.abs]

A callable object can be used just like a normal Racket function:

@examples[#:label #f #:eval pe
          (map b.abs '(1 -2 3 -4))]

To use functions from the Python standard library, you need to import it before
you can use it. The standard library @python-docs["sys"]{sys} provide
a lot of system information. Let's use it to find the version of the Python interpreter.

@examples[#:label #f #:eval pe
          (import sys)
          sys.version_info]

The list of modules in
@hyperlink["https://docs.python.org/3/library/index.html"]{The Python Standard Library}
is long, so let's just try one more.

We want to print a text calendar for the current month.

@margin-note{Documentation for @hyperlink["https://docs.python.org/3/library/calendar.html"]{@tt{calendar}}.}
@examples[#:label #f #:eval pe
          (import calendar)
          (calendar.TextCalendar)
          (displayln ((calendar.TextCalendar) .formatmonth 2022 7))]

The expression @tt{(calendar.TextCalendar)} instantiates a @tt{TextCalendar} object.
The syntax @tt{(object .method arg ...)} is used to invoke the method @tt{formatmonth}
with the arguments 2022 and 7 (for July).

The documentation for @tt{formatmonth} shows its signature:

@centered{@tt{formatmonth(theyear, themonth, w=0, l=0)}}

The two first arguments @tt{theyear} and @tt{themonth} are postional arguments
and the two last arguments @tt{w} and @tt{l} are keyword arguments both has
0 has as default value.

The keyword argument @tt{w} specifies the width of the date columns.
We can get full names of the week days with a width of 9.

@examples[#:label #f #:eval pe
          (displayln ((calendar.TextCalendar) .formatmonth 2022 7 #:w 9))]


@subsection{Exceptions}

An exception on the Python side is converted to an exception on the Racket side.
The exception will be printed with a full traceback.

@examples[#:label #f #:eval pe
          (eval:error (run "1/0"))]


@section{Initialization of the Python Interpreter}

@subsection{The Big Picture}

In order to use the functions in @racket[pyffi] you need to start a Python interpreter.
The call @racket[(initialize)] does just that. A standard approach would be to
make the call in your "main" module of your program.

That the Python interpreter isn't available until the main module has been instantiated
leads to a few complications. The problem is the interpreter instance is not available
when the modules required by "main" is instantiated.

As an example: The module @racket[pyffi/numpy] contains bindings for NumPy.
If the main module looks like this:

@nested[#:style 'inset]{
  @verbatim{#lang racket
            (require pyffi pyffi/numpy)
            (initialize)}}

Then @racket[pyffi/numpy] is instantiated before the interpreter is started.
This means @racket[pyffi/numpy] can't use inspect the Python module @tt{numpy}
to get the function signatures it needs.

To solve this problem @racket[pyffi/numpy] registers a number of initialization
thunks to be run after the interpreter has started. The function
@racket[post-initialize] runs these initialization thunks.

To sum up, the typical main module for a program that uses @tt{pyffi} starts:

@nested[#:style 'inset]{
  @verbatim{#lang racket
            (require pyffi pyffi/numpy)
            (initialize)
            (post-initialize)}}

@subsection{Reference}


@defproc[(initialize) void?]{
Starts a Python interpreter using @tt{libpython}.

The precise steps taken are:

@itemlist[
  @item{The locations of @tt{libpython} and the folder containing
        the standard library is fetched from the preferences 
        (keys @racket['pyffi:libdir] and @racket['pyffi:data]).}
  @item{Calls @python-docs["Py_Initialize"]{Py_Initialize}
        which starts an Python interpreter. Initializes the
        table of of loaded modules and creates the modules @tt{builtins},
        @tt{__main__} and @tt{sys}.}
  @item{Imports @tt{__main__}, @tt{builtins}, @tt{operator}, @tt{traceback} and @tt{inspect}.}
  @item{Creates instances of @tt{True}, @tt{False, and, @tt{None}.}}]
}

@defproc[(post-initialize) void?]{
Run initialization thunks that needs a running Python instance.
}

@defproc[(add-initialization-thunk [thunk thunk?]) void?]{
Add a thunk to be run by @racket[post-initialize].
}

@defproc[(diagnostics) void?]{
Print important Python paths.
}


@section{Evaluation}

@defproc[(run [string-to-evaluate string?]) obj?]{
Evaluate the string @racket[string-to-evaluate] in the running Python interpreter.
The string must contain a Python expression. 
The expression is evaluated in the "main" module.

The resulting Python object is converted to a Racket value via @racket[pr].

If an exception is triggered on the Python side, a Racket exception is 
raised containing the error message and the traceback.

@examples[#:label #f #:eval pe
          (run "[1+10, '2'+'20', (3,30), {4: 40}]")
          (eval:error (run "x = 'this is not an expression'"))]
}

@defproc[(run* [string-to-evaluate string?]) void?]{
Evaluate the string @racket[string-to-evaluate] in the running Python interpreter.
The string must contain a Python statement.
The statement is executed in the "main" module.

If an exception is triggered on the Python side, a Racket exception is 
raised containing the error message and the traceback.

@examples[#:label #f #:eval pe
          (run* "x = 1+10")
          main.x]
}






@;;; ;{
@;; @subsection{Under the hood}

@;; The Python interpreter is available as shared library @tt{libpython}
@;; The shared library provides the (almost) all of the functions described in
@;; @hyperlink["https://docs.python.org/3/c-api/index.html"]{Python/C API Reference Manual}
@;; }

@index-section[]


