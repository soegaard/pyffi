#lang racket
(require pyffi)
;;;
;;; Pillow
;;;

;; The Python library Pillow is the successor of PIL (Python Imaging Library).
;; The example below uses the "P" mode of `convert` to convert a logo
;; into a png with a pallette of only 256 colors.

;;    https://pillow.readthedocs.io/en/stable/index.html


;; Setup Python

(initialize)
(finish-initialization)

;; Import Pillow

(import PIL)
(import-from PIL Image) ; Imports the submodule `Image`

;; Load image
(define im (Image.open "logo.png"))  ; has a gradient

;; Image Info
(displayln (list im.format im.size im.mode))
; Note: The format is only set for images loaded from disk.
;       For images created by the library, the format is set to None.

;; Convert to palette mode and save
(define im2 (im.convert "P")) ; the "P" mode uses a pallette [default is 256 colors]
((im.convert "P") .save "result.png")
(displayln (list im2.size im2.mode))

; (im2.show)  ; show the image in default image viewer
