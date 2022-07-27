#lang racket/base
(require pyffi)

;;;
;;; PYGMENTS
;;;

;; Pygments is a generic syntax highlighter.

;; Example from https://pygments.org/docs/quickstart/


;; from pygments import highlight
;; from pygments.lexers import PythonLexer
;; from pygments.formatters import HtmlFormatter

;; code = 'print "Hello World"'
;; print(highlight(code, PythonLexer(), HtmlFormatter()))


;; Setup Python
(initialize)                
(finish-initialization)

;; Import Pygments
(import-from pygments            highlight)
(import-from pygments.lexers     PythonLexer)
(import-from pygments.formatters HtmlFormatter)

;; Use Pygments
(define code "print 'Hello World'")
(displayln (highlight code (PythonLexer) (HtmlFormatter)))

;; Get the stylesheet
; (displayln ((HtmlFormatter) .get_style_defs ".hightlight"))

;; from pygments.lexers import get_all_lexers
;; (import-from pygments.lexers get_all_lexers)
;; (define i (get_all_lexers))
;; (i.__next__)
