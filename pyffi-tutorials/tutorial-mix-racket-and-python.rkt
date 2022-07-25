#lang racket/base
(require pyffi)

;;;
;;; Mix Racket and Pything
;;;

;; Setup Python
(set-environment-variables)
(initialize)                
(finish-initialization)


(pr (run* "x = 42"))
;(pr (run "x"))

