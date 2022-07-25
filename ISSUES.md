Open
====

Issue 1 - Keywords in function calls 
------------------------------------
- Calling a function `foo` with a keyword not handled by the function 
  results in an exception on the Python side.
  Check the keywords on the Racket side.

This is partially resolved, since Racket now catches the Python exception.


Issue 3 - Reference Counting
----------------------------
Register a will for `obj` values that hold Python objects.
The will executor must decrease the reference counter, so
the Python side can reclaim the value.


Issue 4 - Conversion between Racket and Python values
-----------------------------------------------------
Change the automatic conversion of function arguments to:
  - [done] only convert non-compound result values
  - [    ] handle reference counting for arguments converted from Racket values

Issue 5 - Callbacks
-------------------
Passing a Racket function to Python for use as a callback
needs to be implemented.

Issue 8 - Configuration
=======================

Issue 9 - Documentation
=======================

Issue 10 - Python on the Package Server
=======================================


Resolved
========

Issue 2 - Exceptions on the Python side
---------------------------------------
Handle exceptions raised on the Python side.


Issue 6 - Source location information lost for `run`
----------------------------------------------------
Rather than reporting "<string>" a better source location is needed.

  src-loc: provoke0: Python exception occurred;
    ZeroDivisionError: division by zero
      File "<string>", line 1, in provoke0

Issue 7 - Dotted named and modules implemented via C Extensions
---------------------------------------------------------------
For Python modules that support introspection via `inspect.signature`
we can automatically create Racket functions with the correct number
of positional and keyword arguments. Some Python modules implemented
as C extensions also support this introspection, but some don't.

In particular Numpy functions in general don't support this.

In "numpy.rkt" and "numpy-core.rkt" we have Racket identifiers such as
`numpy.shape` which clash with the automatic handling of dotted names.

Idea: Have a list of names that prevent automatic handling of dotted named.

Say after
  > (declare-special-prefix numpy)
then
  > (numpy.shape A)
will use `numpy.shape` and not `((getattr numpy "shape") A)`.



 
