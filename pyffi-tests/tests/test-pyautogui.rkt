#lang racket
(require pyffi)

;; Setup Python
(initialize)                
(finish-initialization)


;; Import `pyautogui`
(import pyautogui)

;; Use it
(pyautogui.position)
(pyautogui.write "echo \"Hello World\" \n")
