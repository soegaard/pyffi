#lang racket/base
(require pyffi)

;;;
;;; Mix Racket and Pything
;;;

;; Setup Python
(initialize)                
(finish-initialization)


(pr (run* "x = 42"))
;(pr (run "x"))

