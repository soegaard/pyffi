#lang racket
(require (for-syntax racket/base syntax/parse racket/syntax))

(require "../python.rkt")
(set-environment-variables)
(initialize)                
(finish-initialization)

;(run* "import pyautogui")
;(finish-initialization)

; (import pyautogui)

#;(define-syntax (define-functions stx)
  (syntax-parse stx
    [(_ qualifier:id id:id ...)
     (define qualifier-str   (symbol->string (syntax-e #'qualifier)))
     (define qualified-names (for/list ([id (syntax->list #'(id ...))])
                               (format-id id (string-append qualifier-str ".~a") id)))
     (with-syntax ([(qualified-name ...) qualified-names])
       (syntax/loc stx
         (define-delayed
           (define qualified-name
             (begin #;(displayln 'qualified-name)
                    (get-fun 'qualified-name)))
           ...)))]))

;;; Constants

;;   DARWIN_CATCH_UP_TIME FAILSAFE FAILSAFE_POINTS FailSafeException
;; G_LOG_SCREENSHOTS_FILENAMES ImageNotFoundException KEYBOARD_KEYS
;; KEY_NAMES LEFT LOG_SCREENSHOTS LOG_SCREENSHOTS_LIMIT MIDDLE
;; MINIMUM_DURATION MINIMUM_SLEEP PAUSE PRIMARY Point PyAutoGUIException
;; QWERTY QWERTZ RIGHT SECONDARY Size

;; __builtins__ __cached__ __doc__ __file__ __loader__ __name__
;; __package__ __path__ __spec__ __version__ _bottom
;; _genericPyAutoGUIChecks _getCommaToken _getNumberToken
;; _getParensCommandStrToken _getQuotedStringToken _handlePause
;; _logScreenshot _mouseMoveDrag _normalizeButton _normalizeXYArgs
;; _pyautogui_osx _right _runCommandList _snapshot _tokenizeCommandStr

;;; Functions

#;(define-functions pyautogui  
  alert center click confirm contextmanager countdown
  displayMousePosition doubleClick drag dragRel dragTo easeInBack
  easeInBounce easeInCirc easeInCubic easeInElastic easeInExpo
  easeInOutBack easeInOutBounce easeInOutCirc easeInOutCubic
  easeInOutElastic easeInOutExpo easeInOutQuad easeInOutQuart
  easeInOutQuint easeInOutSine easeInQuad easeInQuart easeInQuint
  easeInSine easeOutBack easeOutBounce easeOutCirc easeOutCubic
  easeOutElastic easeOutExpo easeOutQuad easeOutQuart easeOutQuint
  easeOutSine failSafeCheck getInfo getPointOnLine grab hold hotkey
  hscroll isShiftCharacter isValidKey keyDown keyUp leftClick linear
  locate locateAll locateAllOnScreen locateCenterOnScreen locateOnScreen
  locateOnWindow middleClick mouseDown mouseInfo mouseUp move moveRel
  moveTo onScreen password pixel pixelMatchesColor position press
  printInfo prompt raisePyAutoGUIImageNotFoundException rightClick run
  screenshot scroll size sleep tripleClick typewrite
  useImageNotFoundException vscroll write)

; initialize before use
; (finish-initialization)


;; To get the list of function names.

;; (define pyautogui (python->racket (get 'pyautogui)))
;; (define attrs (builtins.dir pyautogui))
;; (for ([a attrs])
;;   (define val (getattr pyautogui a #f))
;;   (when val
;;     (when (obj? val)
;;       (when (inspect.isfunction val)
;;         (displayln a)))))

(import pyautogui)
(pyautogui.position)
;(pyautogui.write "echo \"Hello World\" \n")

(define pos pyautogui.position)
pos
(pos)
