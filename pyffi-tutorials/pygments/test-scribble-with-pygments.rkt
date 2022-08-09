#lang scribble/manual
@(require scribble/core)
@(require pyffi)
@(define py #f)

@(define (exact . items)
   (make-element (make-style "inbox" '()) items))

@(let ()
   ; Note: Using (let () ...) is important here.
   ;       If (begin ...) is used, the scribble moves definitions
   ;       from import-from above the initialization - which
   ;       provokes a crash.
   (initialize)
   (post-initialize)
   (import-from pygments            highlight)
   (import-from pygments.lexers     PythonLexer)
   (import-from pygments.formatters HtmlFormatter)
   (set! py (λ (code)
              (exact (pystring->string (highlight code (PythonLexer) (HtmlFormatter))))))
   (void))

Hello

@exact{foo}
@exact{<h1>Bar</h1>}

@py{print 'Hello World'}



